
-- -p config
import Config.YAML
import Config.JSON
-- -p effects
-- QUESTION/DISCUSSION
-- importing Effects appears to make code (even simple-ish
-- code) take much longer to compile. see
-- https://github.com/benclifford/idris-hang-1
import Effects

-- QUESTION/FOR DISCUSSION:
-- omitting this import of Effect.File gives an unexpected
-- error message to do with type matching. Presumably it not
-- knowing that FILE is an effect that can be run is causing
-- this but it is unexpected to me.
{-
When checking argument env to function Effects.run:
        Type mismatch between
                Env m [] (Type of [])
        and
                Env IO [FILE ()] (Expected type)
        
        Specifically:
                Type mismatch between
                        []
                and
                        [FILE ()]
-}
import Effect.File

import Effect.Exception
import Effect.StdIO

import Todaybot.Curl
import Todaybot.CurlEffect
import Todaybot.Date
import Todaybot.Morph
import Todaybot.Ptr
import Todaybot.TitleParser

-- TODO: put this in the config file
-- and thread that config around suitably.
subredditName : String
-- subredditName = "todaybot_test"
subredditName = "LondonSocialClub"

-- TODO: for every use of unsafePerformIO, note why I believe it is
-- safe.

%include C "ffitest.h"
%link C "ffitest.o"

-- QUESTION/DISCUSSION: compare with the xxxToJSON functions
-- that I have implemented elsewhere. YAML was (I think?) supposed
-- to be treatable as JSON so from that direction, this should
-- perhaps look like something that does yaml->json values (or the
-- yaml parser returns JSON) and then we extract from JSON data
-- types.
--
-- Also the naming is a bit ugly for something globally exposed.
partial uns : YAMLNode -> String
uns (YAMLString s) = s
uns (YAMLScalar s) = s

TodaybotError : Type
TodaybotError = String

partial shred_config : (Either ConfigError YAMLNode) -> Eff (List (String, String)) [STDIO, EXCEPTION TodaybotError]
shred_config config =
  case config of
    Right yamlDoc =>
      do putStrLn "Got YAMLNode..."
         case yamlDoc of
           YAMLDoc _ yamlMap =>
             do putStrLn " ... which is a YamlDoc"
                case yamlMap of
                  YAMLMap m => pure $ map (\(a,b) => (uns a, uns b)) (m)
                -- yamlMap is YAMLMap (List (YAMLNode, YAMLNode))
    Left configError =>
      do putStrLn "config error"
         printLn configError
         raise "shred_config: error shredding configuration file"


partial loadConfig : Eff (List (String, String)) [FILE (), STDIO, EXCEPTION TodaybotError]
loadConfig = do
  config <- readYAMLConfig "secrets.yaml"
  putStrLn "Config is:"
  printLn config

  config_map <- shred_config config

  putStrLn "config shredded to map:"
  printLn config_map

  pure config_map


{- QUESTION/DISCUSSION:

when an effect is missing from the list, this is the kind of error
we get:

ffitest.idr:67:18:
When checking right hand side of loadConfigEff with expected type
        EffM m
             (Either ConfigError YAMLNode)
             [FILE ()]
             (\v => [FILE ()])

When checking argument prf to function Effects.lift:
        Can't find a value of type 
                SubList [STDIO] [FILE ()]

which comes from using a STDIO effect (StdIO.putStrLn) inside
a 'do' block which is only typed as [FILE ()]

Adding STDIO to the type signature of loadConfigEff makes this work.
-}

partial get_access_token : Eff String [STDIO, FILE (), EXCEPTION String, CURL, MEMORY]
get_access_token = do

  -- QUESTION/DISCUSSION this uses effects, but should I spread
  -- the use of effects out to the rest of the program?
  -- Probably, yes - to get away from using IO everywhere, like
  -- in the Haskell version.

  putStrLn "reading config"

  -- QUESTION/FOR DISCUSSION
  -- I attempted to use the same syntax as the Haskell
  -- todaybot, but I get a YAML parse error. I needed to
  -- add:

{-
%foo bar
---
-}

  -- onto the start because it insists on having a yaml directive
  -- at the start... which is the correct behaviour?

  -- Also, app_id and app_secret had to have the underscores removed
  -- The parsing *silently* (!) stopped parsing at that point rather
  -- than (eg.) giving an error that we stopped parsing before the
  -- end of the file or that the symbol is invalid. (are they invalid?)

  config_map <- loadConfig


-- QUESTION/DISCUSSION: using "Just username" in this let binding
-- causes idris command to run for at least 4 minutes (possibly
-- forever?). but fromJust form takes about 10s to compile.
-- (also later I've encountered a similar set of symptoms, but
--  not using a Just LHS pattern...)
  let username = fromJust $ lookup "username" config_map

  -- let (Just username) = lookup "username" config_map

  let password = fromJust $ lookup "password" config_map
  let app_id = fromJust $ lookup "appid" config_map
  let app_token = fromJust $ lookup "appsecret" config_map

  putStrLn "looked up username:"
  printLn username

  -- now init an easy session, giving an easy handle.

  putStrLn "Initialising easy session"

  -- QUESTION/DISCUSSION
  -- Trying to put these two into a single `run $ do` block does not
  -- work, but pulling them both out into a separate function and
  -- putting an explicit type signature on that function does - so
  -- apparently the type signature is needed, but because there are no
  -- inline type signatures, we have to separate it into a separate
  -- function.

  -- making an inline 'quux' with the same type signature as
  -- when defined as a top level function does not work.
{-
  let quux = do
               h <- CurlEffect.curlEasyInit
               checkPointerNotNull h
               pure h

  easy_handle <- run $ the (Eff Ptr [CURL, EXCEPTION String]) quux
-}

  easy_handle <- curlEasyInit
  checkPointerNotNull easy_handle

  ret <- curlEasySetopt easy_handle CurlOptionUrl "https://www.reddit.com/api/v1/access_token"
  checkCurlRet ret

  ret <- curlEasySetopt easy_handle CurlOptionUserAgent "idris-todaybot DEVELOPMENT/TESTING by u/benclifford"
  checkCurlRet ret

  ret <- curlEasySetopt easy_handle CurlOptionUserPwd (app_id ++ ":" ++ app_token)
  checkCurlRet ret

  ret <- curlEasySetopt easy_handle CurlOptionCopyPostFields ("grant_type=password&username=" ++ username ++ "&password=" ++ password)
  checkCurlRet ret

  ret <- curlEasySetopt easy_handle CurlOptionVerbose 1
  checkCurlRet ret


  -- TODO: callback that will get the output and do something with
  -- it (make it into an idris String, for example, so that we can
  -- then parse it).

  -- this is a bit weird because it is some unsafePerformIO style
  -- callback ... but I want to be left with the accumulated state
  -- at the end...

  -- how do I want to/can I accumulate state in idris in IO? does
  -- it have IORefs? what are the other options?

  -- perhaps I should accumulate it in the C side and let there
  -- be a memory leak/resource management problem for now?
  -- and then be able to grab it after the perform?

  -- perhaps an orthogonal (to libcurl) memory buffer allocation
  -- library? (uniqueness types look interesting but not sure if they
  -- fit in with this model of passing in addresses to C libraries?)

  ret <- curlEasySetopt easy_handle CurlOptionWriteFunction write_callback
  checkCurlRet ret

  -- TODO: replace 16 with sizeof a Ptr. but 16 should be big
  -- enough for now.
  content_buf_ptr <- alloc_bytes 16
  checkPointerNotNull content_buf_ptr
  poke_ptr content_buf_ptr null_pointer
  ret <- curlEasySetopt easy_handle CurlOptionWriteData content_buf_ptr
  checkCurlRet ret

  -- QUESTION/DISCUSSION:
  -- that "content_buf_ptr" is a complex structure not just a pointer,
  -- from a memory management perspective, because it potentially contains
  -- a pointer to another heap buffer: when we release content_buf_ptr
  -- we also need to release the heap buffer pointed to.
  -- It can't be released when content_buf_ptr is out of scope because
  -- (potentially) it goes out of scope before easy perform has happened
  -- and so easy perform would write to uninitialised memory.
  -- so for now do manual deallocation of the whole thing.


  -- ugh! this can't wrap a function, for some reason ... it's
  -- saying "idris: Idris function couldn't be wrapped."
  -- I don't know why entirely: the C function should be available
  -- at the call to write_callback, but it looks like the actual
  -- function needs to be known in the line where 'foreign' is
  -- called, rather than it being passed around? QUESTION/DISCUSSION
  -- so I'm only going to be able to statically set a write function?

  -- or can I use "%wrapper" to get the address at run time and then
  -- pass that in as write_callback?

  putStrLn "Performing easy session"

  ret <- curlEasyPerform easy_handle
  checkCurlRet ret

  -- QUESTION/DISCUSSION: can this release happen through
  -- garbage collection? should it? (it will shut network
  -- connections as well as release memory - so maybe that
  -- shouldn't happen, and instead an explicit 'with' block
  -- should wrap the curl interactions, perhaps using
  -- Effects for this?)

  curlEasyCleanup easy_handle


  -- at this point, content_buf_ptr should be a **content
  -- reference that we can somehow cast into a string
  -- and then parse as JSON. and then leave to the winds
  -- (or rather, some kind of cleardown point that we believe
  -- it is safe to unalloc?) - regions/linear types?


  dump_buffer content_buf_ptr

  -- QUESTION/DISCUSSION: what's the right/best way to get an
  -- idris string out of this buffer? (there might be encoding
  -- issues?)

  -- according to a question I (bxc) asked about releasing memory
  -- on #idris:
{-
12:04 < Melvar> bxc: It copies the string, because a C string is mutable 
                and could change with the next foreign call, but an Idris 
                string is immutable.
12:06 < bxc> OK. So (assuming the C code doesn't want to make further use 
             of it) it should be ok for me to free() it right after.
-}

  response_body <- cast_to_string content_buf_ptr

  putStrLn "idris-side: buffer string is: "
  putStrLn response_body

  content_buf <- peek_ptr content_buf_ptr
  free content_buf
  free content_buf_ptr


  -- TODO: parse out the access_token JSON object field.
  -- TODO: could do with a JSON parser here... there's one inside
  -- the config module, though...

  let asJSON = Config.JSON.fromString response_body


  putStrLn "buffer as json:"
  printLn asJSON

  -- QUESTION/COMMENT: using this case as a let, and not returning
  -- IO actions doesn't seem to work for me, with an incomplete
  -- term error... not sure why.
{-

QUESTION/COMMENT: in this code sample it doesn't seem to figure
out that j should have a JSON type, and the println fails with
Can't find implementation for Show a

(which i think means it isn't figuring out that a should be a JSONValue
type coming from the type of dict?)

doing pattern matching differently ends up working, but I'm not
sure why this doesn't work...

  j <- case asJSON of
            Right (JsonObject dict) => pure $ Data.AVL.Dict.lookup "access_token" dict
            -- _ => pure "IS OTHERWISE"  -- TODO handle this better (with error monad? / effect?)

  putStrLn "j is:"
  printLn j

-}

  let (Right (JsonObject dict)) = asJSON -- TODO error handling on non-match case
 
  let (Just (JsonString access_token)) = Data.AVL.Dict.lookup "access_token" dict
 
  putStrLn "access_token is:"
  putStrLn access_token

  pure access_token

get_hot_posts : String -> Eff String [CURL, EXCEPTION String, MEMORY, STDIO]
get_hot_posts access_token = do
  -- now we can make calls to oauth.reddit.com using the access token

  -- getHotPosts using libcurl.

  easy_handle <- curlEasyInit

  -- TODO: better abstractions for this slist? Can we do it functionally
  -- using unsafePerformIO? and using a better pointer type rather than
  -- Ptr.

  slist <- curlSListAppend null_pointer ("Authorization: " ++ "bearer " ++ access_token)

  -- TODO: factor this for calling on any http request
  ret <- curlEasySetopt easy_handle CurlOptionWriteFunction write_callback
  checkCurlRet ret

  content_buf_ptr <- alloc_bytes 16
  checkPointerNotNull content_buf_ptr
  poke_ptr content_buf_ptr null_pointer

  ret <- curlEasySetopt easy_handle CurlOptionWriteData content_buf_ptr
  checkCurlRet ret

  -- TODO: factor into "set todaybot useragent header"
  ret <- curlEasySetopt easy_handle CurlOptionUserAgent "idris-todaybot DEVELOPMENT/TESTING by u/benclifford"
  checkCurlRet ret

  ret <- curlEasySetopt easy_handle CurlOptionHttpHeader slist
  checkCurlRet ret

  ret <- curlEasySetopt easy_handle CurlOptionUrl ("https://oauth.reddit.com/r/" ++ subredditName ++ "/hot?limit=30")
  checkCurlRet ret

  putStrLn "Performing easy session (2)"

  ret <- curlEasyPerform easy_handle
  checkCurlRet ret

  curlEasyCleanup easy_handle
  -- slist is not allowed to be released until after the handle
  curlSListFreeAll slist

-- DISCUSSION:
-- with everything up to here mostly in a big main function
-- rather than separated into eg get_access_token:
-- at least 3m19s compile time (aborted by me),
-- with the above printLn ret in
-- but the below dump buffer out.
-- With below dump buffer in, bu printLn out at least 4m22s aborted
-- by me.
-- With neither, compile time is about 1m20s
-- With both, real time before I aborted was 16m47s, but only 1m41s cpu time
-- with the rest (by the sound of my HD) being used in swapping...
-- so only a few seconds extra progress... maybe there's some serious
-- memory usage that can be diddled in the idris compiler?
-- I can possibly can look at that?
-- By pulling out some of the code into its own function,
-- get_access_token, real time goes down to 1m1s even with both
-- above and below lines in, which unblocks me for now and points
-- in the direction, perhaps, that it is large do blocks that are
-- causing a problem?

  dump_buffer content_buf_ptr

  response_body <- cast_to_string content_buf_ptr

  content_buf <- peek_ptr content_buf_ptr
  free content_buf
  free content_buf_ptr

  pure response_body

-- given a JSON object, looks up the named key returning Nothing
-- if it does not exist.
-- for a json value that is not an object, indicate failure
-- by Nothing.
-- QUESTION/DISCUSSION: this might not be the right error
-- handling: if I believe a key will be there (because I know
-- reddit's schema) I perhaps want a more "serious" error report?
getkey : String -> JsonValue -> Maybe JsonValue
getkey key (JsonObject dict) = lookup key dict
getkey key _ = Nothing


-- potentially this could return a List rather than a (Maybe . List)
-- but I want to keep a distinction between calling arrayFromJSON
-- on the wrong kind of JsonValue vs getting an empty array in the
-- right kind of JsonValue.
arrayFromJSON : JsonValue -> (Maybe . List) JsonValue
arrayFromJSON (JsonArray a) = Just a
arrayFromJSON v = Nothing

stringFromJSON : JsonValue -> Maybe String
stringFromJSON (JsonString s) = Just s
stringFromJSON v = Nothing


{- QUESTION/DISCUSSION:
arrayFromJSON : JsonValue -> Maybe [JsonValue]
arrayFromJSON v = Nothing
gives:
 `-- ffitest.idr line 601 col 14:
      When checking type of Main.arrayFromJSON:
           When checking argument a to type constructor Prelude.Maybe.Maybe:
                        Can't disambiguate since no name has a suitable type:
                                             Effects.Env.::, Prelude.List.::, Prelude.Stream.::, D\
                                             ata.Vect.::
                                             

where line 601, col 14 is the space before the : in the type signature

LATER:
This comes, I think, from [list] syntax not being recognised and
List JsonValue being needed instead.

This is an awkward error message...

I guess what is happening is it is creating a list of one element
(or attempting to) to pass into Maybe. Because [] notation is for
list values not list types.

-}

-- partial due to 'fromJust'
partial forceFlair : String -> JsonValue -> String -> String -> Eff () [STDIO, CURL, EXCEPTION String, MEMORY]
forceFlair access_token post new_flair new_css_class = do

  let fullname = fromJust $ do  
    kind <- (getkey "kind" post) >>= stringFromJSON
    dat <- getkey "data" post
    ident <- (getkey "id" dat) >>= stringFromJSON
    pure (kind ++ "_" ++ ident)
    -- QUESTION/DISCUSSION:
    -- if I use 'return' here, the deprecation warning for it
    -- is emitted twice.

  -- QUESTION/DISCUSSION: does curl connection sharing only
  -- happen on the same handle? if so it would be advantageous
  -- to share the handle for flair setting with the handle used
  -- for retrieving the post list.
  easy_handle <- curlEasyInit
  checkPointerNotNull easy_handle

  -- TODO: all these rets need testing.

  ret <- curlEasySetopt easy_handle CurlOptionUrl ("https://oauth.reddit.com/r/" ++ subredditName ++ "/api/flair")
  checkCurlRet ret

  ret <- curlEasySetopt easy_handle CurlOptionUserAgent "idris-todaybot DEVELOPMENT/TESTING by u/benclifford"
  checkCurlRet ret


  ret <- curlEasySetopt easy_handle CurlOptionCopyPostFields ("api_type=json&link=" ++ fullname ++ "&text=" ++ new_flair ++ "&css_class=" ++ new_css_class)
  checkCurlRet ret

  ret <- curlEasySetopt easy_handle CurlOptionVerbose 1
  checkCurlRet ret

  slist <- curlSListAppend null_pointer ("Authorization: " ++ "bearer " ++ access_token)
  checkPointerNotNull slist

  ret <- curlEasySetopt easy_handle CurlOptionHttpHeader slist
  checkCurlRet ret

  ret <- curlEasySetopt easy_handle CurlOptionWriteFunction write_callback
  checkCurlRet ret

  content_buf_ptr <- alloc_bytes 16
  checkPointerNotNull content_buf_ptr

  poke_ptr content_buf_ptr null_pointer
  ret <- curlEasySetopt easy_handle CurlOptionWriteData content_buf_ptr
  checkCurlRet ret

  ret <- curlEasyPerform easy_handle
  checkCurlRet ret

  content_buf <- peek_ptr content_buf_ptr
  free content_buf
  free content_buf_ptr

  curlEasyCleanup easy_handle
  curlSListFreeAll slist

  putStrLn "End of forceFlair"


-- QUESTION/DISCUSSION:
-- This got factored into its own function because I found
-- that it was easier to diagnose type problems by having
-- more explicit type signatures on functions (rather than
-- letting them be inferred inside a big main and getting
-- an error right at the start of main)

-- TODO: This way of writing nested unwrapping of maybes is ick. Write
-- it better.
get_all_hot_posts : Maybe JsonValue -> List JsonValue
get_all_hot_posts psm = case psm of
  Nothing => []
  Just ps => case arrayFromJSON ps of
    Just l => l
    Nothing => []

-- TODO: access_token should be in some kind of environment.
partial processPost : String -> JsonValue -> Eff () [STDIO, TIME, EXCEPTION String, MEMORY, CURL]
processPost access_token post = do

  -- now extract the heading from this:
  -- in /data/

  let posttitle = do
        postdata <- getkey "data" post
        js <- getkey "title" postdata
        stringFromJSON js

  let postflair = do
        postdata <- getkey "data" post
        text <- (getkey "link_flair_text" postdata) >>= stringFromJSON
        css <- (getkey "link_flair_css_class" postdata) >>= stringFromJSON

        -- QUESTION/DISCUSSION: the above notation would look nicer
        -- with =<< I think (keeping data flowing R->L)

        -- QUESTION/DISCUSSION: I hit that annoying error reporting
        -- style here again, where I had miscapitalised stringFromJSON
        -- and instead of a "name not found" error I got that
        -- idris couldn't pick a >>= from the top line of main. Is
        -- idris trying to use the choice of >>= to decide what to
        -- suggest/lookup stringFromJson?
        
        pure (text,css)

  putStrLn "Post title:"
  printLn posttitle
  putStrLn "Post flair:"
  printLn postflair

  let postdate = posttitle >>= titleToDate
  putStrLn "Post date:"
  printLn postdate

  -- next, what is the current date? (we don't need the time
  -- of day, but we do need it to be accurate in the local
  -- timezone so that we use London time rather than eg UTC)
  -- In the Haskell version, there is 'getCurrentLocalTime' which
  -- uses both getCurrentTime (giving UTC) and 'getCurrentTimeZone'
  -- which returns the current TZ definition; and then uses
  -- utcToLocalTime to combine the results of those two IO actions.

  -- what do glibc calls for time actually look like? because I'll
  -- probably be using those.
  
  -- 'struct tm' is the broken down time structure to use
  -- localtime_r is a thread safe / memory safe function which
  -- can give a 'struct tm' given a simple time.
  
  -- use 'time' to get a time_t value. (or we can write it into
  -- a buffer if using a time_t raw value is awkward?)

  -- then once we have our 'struct tm' we should have a way to
  -- convert it into the same Date as used in the time parser
  -- so that 'Eq' can be used.

  now <- getTime
  
  putStrLn "Current time, as TimeT:"
  printLn now

  nowDate <- timeTToDate now
  putStrLn "Current time, as Date:"
  printLn nowDate

  -- so now we have flair, and dates. we can do some state transition
  -- rules thing to decide what posts get changed.

  -- the rules are:

  -- the basic time based transition rules:
  -- is this post today, with blank flair? => set flair to today
  -- is this post in past, with 'today' or blank flair? => set flair to archived

  -- today but blank flair?
  if (Just nowDate == postdate && postflair == Nothing) 
    then do
      putStrLn "Rule TODAY firing: Set today flair"
      forceFlair access_token post "Today" "today"
    else putStrLn "Rule TODAY not firing"


  -- in the past but (still) has today flair?
  let inpast = do
        d <- postdate
        pure (d < nowDate)

-- QUESTION/DISCUSSION: ^
-- when Date doesn't have an ordering instance, then the
-- frustrating top level >>= disambiguation error appears
-- (and goes away if 'pure (d < nowDate)' is replaced with
-- 'pure False'.

  if inpast == Just True && (  postflair == Just ("Today", "today")
                            || postflair == Nothing )
    then do
      putStrLn "Rule PAST firing"
      forceFlair access_token post "Archived" "archived" 
    else putStrLn "Rule PAST not firing"

  -- there is also a non-date transition:
  -- is this an interest check? (actually, this is distinct from
  -- dated post behaviour...) - If flair is blank, set flair to interest
  -- deal with that later though

  -- I should probably switch to testing with r/benclifford at this
  -- point because soon I want to test actually changing flair on
  -- posts and that shouldn't happen on r/LondonSocialClub

  pure ()

-- QUESTION/DISCUSSION: Much messing round with this type
-- signature. For example, I wanted:
-- sequenceEff : List (Eff () e) -> Eff () e
-- but as Eff programs are typed by the monad that they
-- are over, the hidden monad parameter in the LHSes doesn't
-- unify with the hidden monad in the RHS.
-- EffT is Eff with the monad specified, which means we can
-- explicitly use the same variable on both sides.

-- QUESTION/DISCUSSION: Why does EffM have a monad parameter
-- in it? It 
-- seems to be needed in a constaint for the 'New' EffM
-- constructor, which only lets you introduce effects which
-- have a handler in m, I think. That jars a bit with my
-- expectations of handling effects (which is what? perhaps
-- because I'm also used to plugging in interpreters rather
-- than having them defined once per underlying monad, which
-- I guess is what this constraint is for: saying that
-- it can be handled).

sequenceEff : List (EffT m a e) -> EffT m (List a) e
sequenceEff [] = pure []
sequenceEff (x :: xs) = do
  v <- x
  vs <- sequenceEff xs
  pure (v :: vs)

partial oneshotMain : Eff () [STDIO, FILE (), EXCEPTION String, CURL, MEMORY, TIME]
oneshotMain = do
  access_token <- get_access_token

  -- finally, we're logged in.
  hot_posts <- get_hot_posts access_token

  -- QUESTION/DISCUSSION: the string that is returned from here
  -- is somehow invalid in so much as it causes a SIGSEGV when
  -- either trying to print it with 'print' or parse it using
  -- Config.JSON. So I guess my assumptions that I can just dump
  -- what came back into an idris string is invalid.
  -- LATER:
  -- Actually, valgrind (when I got it working) suggests it is
  -- a stack overflow. Running valgrind with an 80mb (rather than
  -- 8mb) stack makes the code appear to hang for a long time
  -- (but maybe just processing...?)
  -- It should be easy to change the size of the returned hot posts
  -- JSON by asking reddit for fewer posts in the request URL. At
  -- the time of this problem, it is 100 posts.
  -- LATER:
  -- so with a smaller count of posts (eg 1) the parser
  -- instead was dying because Config.JSON master couldn't parse
  -- a broad enough dialect of JSON. So I went and hacked that
  -- into my local checkout of Config.JSON:  string escapes and
  -- null strings being the two things I needed to change, and now
  -- we can parse hot posts as long as we only get 1.
  -- LATER:
  -- informally the parser on a large (100k-ish) input is *way*
  -- slower if there is a syntax error near the end that will
  -- cause ultimate failure, compared to if there is a nice clean
  -- successful parse. Might be interesting to benchmark that?

  -- putStrLn "hot posts as idris string:"
  -- print hot_posts

  let m_hot_posts_as_json = eitherToMaybe (Config.JSON.fromString hot_posts)

  -- maybe this should be an exception effect rather than discarding
  -- the error info?

  putStrLn "hot posts as json:"
  printLn m_hot_posts_as_json

  -- so we have this JSON structure, but now what can we do
  -- with it?

  -- the two approaches I've seen in Haskell are: i) deserialise
  -- into a specific data structure (eg a Post structure for a
  -- reddit post) or pass the JSON data around and access it
  -- by paths into the structure, eg with lenses.
  -- In the main lsc-todaybot implementation, I'm using the
  -- latter, with accessors like this:
  -- postFlairCss = key "data" . key "link_flair_css_class" . _String

  -- Most immediately I'd like to print out a list of the
  -- post titles, without all the fluff.
  -- so for each post, I want data/title as a string.

  -- start with kind Listing, with descendent:
  --   /data/children
  -- and children is an [array] of more reddit objects,
  -- with kind t3, and title /data/title

  -- I'm having trouble here getting >>= to be resolved:
  -- let maybe_hot_post_Listing = m_host_posts_as_json >>= (getkey "data")
  -- main fails to typecheck with 
{-
When checking an application of function Prelude.Monad.>>=:
        Can't disambiguate since no name has a suitable type: 
                Effects.>>=, Prelude.Monad.>>=
)
-}
--   let maybe_hot_post_Listing = m_host_posts_as_json >>= ?foo

-- but this works? (or I made something work by fiddling with
-- type signatures elsewhere?)

  -- let maybe_hot_post_Listing = m_hot_posts_as_json >>= (getkey "foo")
  let maybe_hot_post_Listing = do
         i <- m_hot_posts_as_json
         i2 <- getkey "data" i
         getkey "children" i2
         -- rewrite the above using >>= as a single point free
         -- sequence, not a do block?
         
  -- so hot_post_Listing should be a JSON array.
  -- get that into a list and then if it has any entries,
  -- print out the first one. (although actually I'll want to
  -- do some action over *all* of them)

  putStrLn "maybe_hot_post_Listing = "
  printLn maybe_hot_post_Listing

  -- TODO: factor as maybeHead and then use monadic join?

  {- The below gives a terrible error message, in so much
     as the error occurs way up at the top of main.
     QUESTION/DISCUSSION: committing to version control
     for investigating later. I've encountered this 
     error several times and I wonder if there's a better
     way for me to deal with it / for it to be reported.
     or for me to understand what's happening.

     LATER: looks like the reason was maybe_host_post_Listing
     vs maybe_hot_post_Listing (a typo in hot) which manifests
     as that weird bind error rather than a name not found
     error
-}

  {- QUESTION/DISCUSSION: why does this result in 
Can't find implementation for Show (Maybe b)
  (eg why can't we figure out what the filler type for the
  maybe is?) and all sorts of other different errors, none
  of which have led me to a solution so far.
-}
{-
  let first_post : Maybe Nat
      first_post = do
        l <- maybe_hot_post_Listing
        case 1 of x => return x
-}

  let ps = get_all_hot_posts maybe_hot_post_Listing

  -- QUESTION/DISCUSSION: I fiddled round with this to try to
  -- eliminate the ppppp and do composition, but I got some
  -- errors I didn't understand: I think coming from me being
  -- to used to monads.

  -- WORKS: for ps (\ppppp => run $ processPost access_token ppppp)

  -- DOESNT WORK: for ps (\ppppp => (run . (processPost access_token)) ppppp)
  {- gives the following - as if it can't unify the two type sigs even though
     I think it should be able to? processPost isn't fully applied in the
     second case, and I wonder if that is causing problems because of
     dependent types?

When checking an application of function Prelude.Basics..:
        Type mismatch between
                JsonValue ->
                Eff ()
                    [STDIO,
                     TIME,
                     EXCEPTION String,
                     MEMORY,
                     CURL] (Type of processPost _)
        and
                a1 -> EffM m a xs xs' (Expected type)
-}

  -- DOESN'T WORK: can't commute run and for. But i want to, because
  -- I want to have the 'for' way outside up at main... for needs an
  -- Applicative, and Eff is not that... what is the Eff equivalent
  -- idiom for doing this?
  -- run  $ for ps (\ppppp => (processPost access_token ppppp))

  -- The 'Eff' library defines <*> etc but not as Applicatives,
  -- instead using ad-hoc overloading. Which means that things
  -- like 'for' need to be re-defined rather than re-used.
  -- Perhaps something as horrific as a source code copy(?)
  -- for ps (\ppppp => (run (processPost access_token ppppp)))

  let effectful_acts = map (\ppppp => processPost access_token ppppp) ps
  -- for effectful_acts (\ea => run ea)
  sequenceEff effectful_acts
  pure ()

-- QUESTION/DISCUSSION
-- implementing: forever act = act *> forever act
-- (which would work in Haskell) causes a segfault here
-- because it tries to construct the entire (IO originally
-- but now Eff) action
-- (i.e. unrolling an infinite loop). Use of >>= does not
-- construct the second action until the result of the
-- first action is known, in order to pass that value
-- into the constructing function.
-- This is a strictness/laziness issue that I wasn't
-- expecting - maybe it's of interest when thinking about
-- Haskell's ApplicativeDo notation too, because different
-- equivalences apply here.

partial forever : EffT m () e -> EffT m () e
forever act = do
  act
  forever act

-- QUESTION/DISCUSSION this is another case where I seem
-- to be forced to move the effectful 'do' block into its
-- own function with a type signature, rather than
-- being able to specify the type inline with a 'the'
-- annotation like this:
--
-- the (Eff () [TIME, STDIO]) $ do
--
-- At least it can go in a where clause.

sleepAWhile : Eff () [TIME, STDIO]
sleepAWhile = do
  putStrLn "sleep starting"
  sleep (7*60)
  putStrLn "sleep done"


partial main : IO ()
main = do
  putStrLn "idris ffi test start"

  putStrLn $ "calling global init for curl"
  -- TODO: send it proper init code not 3 (extract from lib...)
  ret <- run curlGlobalInit
  printLn ret
  -- TODO: check ret == 0
  putStrLn $ "called global init for curl"

  {- QUESTION/DISCUSION: in relation to other comment about
     needing to put in 'where' clauses for contained do blocks,
     it appears that this particular do block doesn't need one...

  forever $ run $ do
    oneshotMain
    sleepAWhile

  -}

  run $ forever $ do
    oneshotMain
    sleepAWhile

  putStrLn "Shutting down libcurl"
  run curlGlobalCleanup
  putStrLn "idris ffi test end"


-- QUESTION/DISCUSSION
-- valgrind seems to work relatively well (better than I'd expected)
-- when debugging memory use with C bindings. I wonder what could
-- be done to make this kind of trace look better?
-- $ valgrind --leak-check=full ./todaybot 
-- Really I'd like .idr file line numbers on that idris code
-- (not C intermediate representation source lines)
{-
==10505== 14,437,220 (384 direct, 14,436,836 indirect) bytes in 1 blocks are definitely lost in loss record 254 of 254
==10505==    at 0x4C2FB55: calloc (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
==10505==    by 0x530A23F: Curl_multi_handle (in /usr/lib/x86_64-linux-gnu/libcurl-gnutls.so.4.4.0)
==10505==    by 0x530383E: curl_easy_perform (in /usr/lib/x86_64-linux-gnu/libcurl-gnutls.so.4.4.0)
==10505==    by 0x403885: _idris_Todaybot_46_Curl_46_curlEasyPerform (in /home/benc/src/idristodaybot/todaybot)
==10505==    by 0x56127C: _idris_io_95_bind (in /home/benc/src/idristodaybot/todaybot)
==10505==    by 0x56127C: _idris_io_95_bind (in /home/benc/src/idristodaybot/todaybot)
==10505==    by 0x56127C: _idris_io_95_bind (in /home/benc/src/idristodaybot/todaybot)
==10505==    by 0x5A0499: _idris__123_runMain_95_0_125_ (in /home/benc/src/idristodaybot/todaybot)
==10505==    by 0x4025E3: main (in /home/benc/src/idristodaybot/todaybot)

-}

{- QUESTION/DISCUSSION:
after tidying curl and time C heap memory leaks, valgrind detects only
one final leak, coming from idris itself:

==11275== 416 bytes in 1 blocks are still reachable in loss record 1 of 1
==11275==    at 0x4C2DB8F: malloc (in /usr/lib/valgrind/vgpreload_memcheck-amd64-linux.so)
==11275==    by 0x404282: init_vm (in /home/benc/src/idristodaybot/hello)
==11275==    by 0x4020FA: main (in /home/benc/src/idristodaybot/hello)
==11275== 

Maybe that should be freed in idris, for valgrind perfection?


-}
