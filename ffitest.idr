
-- -p config
import Config.YAML
import Config.JSON
-- -p effects
-- QUESTION/DISCUSSION
-- importing Effects appears to make code (even simple-ish
-- code) take much longer to compile. see
-- https://github.com/benclifford/idris-hang-1
import Effects

import Todaybot.Morph
import Todaybot.TitleParser

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


-- TODO: for every use of unsafePerformIO, note why I believe it is
-- safe.

%lib C "curl"
%include C "ffitest.h"
%link C "ffitest.o"

%include C "curl/curl.h"

-- This should be safe because NULL should be a constant.
null_pointer : Ptr
null_pointer = unsafePerformIO $ foreign FFI_C "get_null_pointer" (IO Ptr)

test_ffi_callback : String -> ()
test_ffi_callback s = unsafePerformIO $ do
  putStrLn $ "CALLBACK!!! string=" ++ s

data CurlOption : Type where
  CurlOptionUrl : CurlOption
  CurlOptionUserPwd : CurlOption
  CurlOptionCopyPostFields : CurlOption
  CurlOptionUserAgent : CurlOption
  CurlOptionVerbose : CurlOption
  CurlOptionWriteFunction : CurlOption
  CurlOptionWriteData : CurlOption
  CurlOptionHttpHeader : CurlOption

-- QUESTION/FOR DISCUSSION: these values are defined in
-- curl.h (by a fancy macro that also knows what the
-- types are). i) it would be nice to get the constants,
-- and ii) it would be ever nicer to get the correct types
-- as currently mirrored in curlOptionType.
-- could type providers provide this? (or one of the other
-- compile-time things?) by running C code at compile time
-- for an "import this named option" call run many times?
-- or is there a different way to get the list of defined
-- curl options out of eg. cpp?
total curlOptionToFFI : CurlOption -> Int
curlOptionToFFI CurlOptionUrl = 10002
curlOptionToFFI CurlOptionUserAgent = 10018
curlOptionToFFI CurlOptionUserPwd = 10005
-- use this not postfields, because unlike other string
-- options, POSTFIELDS does not copy the input - so I
-- don't know how it interacts with idris memory management.
curlOptionToFFI CurlOptionCopyPostFields = 10165
curlOptionToFFI CurlOptionVerbose = 41
curlOptionToFFI CurlOptionWriteFunction = 20011
curlOptionToFFI CurlOptionWriteData = 10001
curlOptionToFFI CurlOptionHTTPHeader = 10023

total curlOptionType : CurlOption -> Type
curlOptionType CurlOptionUrl = String
curlOptionType CurlOptionUserAgent = String
curlOptionType CurlOptionUserPwd = String
curlOptionType CurlOptionCopyPostFields = String
curlOptionType CurlOptionVerbose = Int
curlOptionType CurlOptionWriteFunction = Ptr
curlOptionType CurlOptionWriteData = Ptr
curlOptionType CurlOptionHttpHeader = Ptr -- to curl slist

-- a handle to a curl "easy session". might be better to
-- wrap it so that Ptr vs Ptr type errors are detected?
EasyHandle : Type
EasyHandle = Ptr


-- QUESTION/FOR DISCUSSION
-- this is a bit of a workaround for dependent type evaluation
-- not being as I would like in the parameters to
-- "foreign" -- we need a separate case for each concrete
-- invocation parameter type (although I don't think they
-- need to be in separate functions, the calls need to be
-- duplicated with concrete type known at each call)...
-- which is a shame.

-- setoptString doesn't need to be exposed to the rest of the program: it should only be called by curlEasySetopt
total curlEasySetoptString : EasyHandle -> (opt : CurlOption) -> String -> IO Int
curlEasySetoptString easy_handle opt param = do
  ret <- foreign FFI_C "curl_easy_setopt" (Ptr -> Int -> String -> IO Int) easy_handle (curlOptionToFFI opt) param
  -- TODO: check ret
  pure ret

total curlEasySetoptLong : EasyHandle -> (opt : CurlOption) -> Int -> IO Int
curlEasySetoptLong easy_handle opt param = do
  ret <- foreign FFI_C "curl_easy_setopt" (Ptr -> Int -> Int -> IO Int) easy_handle (curlOptionToFFI opt) param
  -- TODO: check ret
  pure ret

total curlEasySetoptPtr : EasyHandle -> (opt : CurlOption) -> Ptr -> IO Int
curlEasySetoptPtr easy_handle opt param = do
  ret <- foreign FFI_C "curl_easy_setopt" (Ptr -> Int -> Ptr -> IO Int) easy_handle (curlOptionToFFI opt) param
  -- TODO: check ret
  pure ret


-- QUESTION/FOR DISCUSSION:
-- encode types into values because apparently we
-- can't use a Type on the LHS of a case?
-- which means I can't match on the type of the option...
-- not sure how to encode this better?

-- Note that the below explicit listing still has type checking
-- protection: if the wront setoptXXXXX function is called, there
-- will be a static type checking error.

total curlEasySetopt : EasyHandle -> (opt : CurlOption) -> curlOptionType opt -> IO Int
curlEasySetopt easy_handle opt param = case opt of
  CurlOptionHttpHeader => curlEasySetoptPtr easy_handle opt param
  CurlOptionUrl => curlEasySetoptString easy_handle opt param
  CurlOptionUserAgent => curlEasySetoptString easy_handle opt param
  CurlOptionUserPwd => curlEasySetoptString easy_handle opt param
  CurlOptionCopyPostFields => curlEasySetoptString easy_handle opt param
  CurlOptionVerbose => curlEasySetoptLong easy_handle opt param
  CurlOptionWriteFunction => curlEasySetoptPtr easy_handle opt param
  CurlOptionWriteData => curlEasySetoptPtr easy_handle opt param

-- === debugging dependent types of setopt
{-
data A : Type where
  A1 : A
  A2 : A

f : A -> Type
f A1 = String
f A2 = Int

inner : EasyHandle -> A -> String -> Int
inner g opt str = 100

inner2 : EasyHandle -> A -> Int -> Int
inner2 h opt val = 101

outer : EasyHandle -> (o : A) -> f o -> Int
outer h opt param = case opt of
  A1 => inner h opt param
  A2 => inner2 h opt param

-}
-- === end setopt type debugging

partial uns : YAMLNode -> String
uns (YAMLString s) = s
uns (YAMLScalar s) = s

partial shred_config : (Either ConfigError YAMLNode) -> IO (List (String, String))
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
         pure []


partial fromJust : Maybe a -> a
fromJust (Just v) = v

-- TODO: this should be captured at compile time
-- eg using type providers as in example
SizeT : Type
SizeT = Int

-- TODO: all this memory stuff should take account of CData
-- which I discovered after implementing this. Specifically
-- it looks like we can do garbage collection, if careful.

-- TODO: memory allocation should probably do some validity checking
-- and have some appropriate exception-effect or some such on
-- failure, rather than returning NULL?

alloc_bytes : SizeT -> IO Ptr
alloc_bytes count = foreign FFI_C "alloc_bytes" (SizeT -> IO Ptr) count

realloc_bytes : Ptr -> SizeT -> IO Ptr
realloc_bytes old count = foreign FFI_C "realloc" (Ptr -> SizeT -> IO Ptr) old count

memcpy : Ptr -> Ptr -> SizeT -> IO Ptr
memcpy dest src count = foreign FFI_C "memcpy" (Ptr -> Ptr -> SizeT -> IO Ptr) dest src count

-- TODO: should be a byte not an int
poke_byte : (base : Ptr) -> (offset : Int) -> (value : Int) -> IO ()
poke_byte base offset value = foreign FFI_C "poke_byte" (Ptr -> Int -> Int -> IO ()) base offset value

-- TODO: inconsistent offset UI wrt poke_byte
poke_ptr : (base : Ptr) -> (value : Ptr) -> IO ()
poke_ptr base value = foreign FFI_C "poke_ptr" (Ptr -> Ptr -> IO ()) base value

peek_ptr : (base : Ptr) -> IO Ptr
peek_ptr base = foreign FFI_C "peek_ptr" (Ptr -> IO Ptr) base

-- QUESTION/DISCUSSION: Really I would like some kind of
-- Eq on pointers. But I think maybe I don't have those?

is_null : Ptr -> Bool
is_null ptr = ptr == null_pointer

{-
is_null : Ptr -> Bool
is_null ptr = do
  v <- foreign FFI_C "is_null" (Ptr -> IO Int) ptr
  case v of
    501 => True
    502 => False
-}


write_callback_body : Ptr -> Int -> Int -> Ptr -> Int
write_callback_body curldata s1 s2 userdata = unsafePerformIO $ do
  putStrLn "In write_callback_body"

  -- TODO: assert s1*s2 != 0 - if it does, then some of the
  -- memory allocation C calls are going to free memory rather
  -- than allocate a 0 byte buffer. I think this is unlikely to
  -- happen in practice as libcurl wouldn't call the callback in
  -- that way, but it is not captured in the types...

  -- TODO: what kind of encoding is this data? That's going
  -- to need to be diddled with but assume it can be treated as a
  -- String by idris directly now (with the addition of a 0 on
  -- the end).

  -- TODO: also for now assume the data will arrive as a single
  -- block which is probably true for the auth response mostly
  -- but not for the bigger responses...

  -- TODO: some of these are byte counts, some might be size_ts?
  -- 

  old_buf <- peek_ptr userdata

  old_length <- case is_null old_buf of
    True => pure 0
    False => foreign FFI_C "strlen" (Ptr -> IO Int) old_buf

  let size_to_alloc = s1 * s2 + old_length + 1
  longterm_buf <- realloc_bytes old_buf size_to_alloc

  -- rather than a memcpy this should be a concatenation
  -- which after one iteration will be fine because we know
  -- that the old buffer will be null terminated;
  -- but in the first iteration, the old_buf didn't even
  -- exist so it couldn't be null terminated.

  -- could switch on the value of old_buf being null or not?
  -- although perhaps it would have been nicer if old_buf always
  -- pointed to a valid null terminated string so that we don't need
  -- to change code-paths here?

  -- TODO: need an offset of old_length for the copy base
  -- (longterm_buf + old_length rather than longterm_buf)
  -- and likewise in the null pointer setting.

  new_base <- foreign FFI_C "add_ptr_offset" (Ptr -> SizeT -> IO Ptr) longterm_buf old_length

  memcpy new_base curldata (s1 * s2)

  -- TODO: can I do pointer arithmetic?
  -- naively s1,s2 and longterm_buf are the wrong types
  -- but perhaps implicit conversions could help?
 
  poke_byte longterm_buf (s1 * s2 + old_length) 0

  poke_ptr userdata longterm_buf

  putStrLn "Done with write_callback_body"
  pure (s1 * s2)

write_callback : Ptr
write_callback = unsafePerformIO $
  foreign FFI_C "%wrapper" (CFnPtr (Ptr -> Int -> Int -> Ptr -> Int) -> IO Ptr) (MkCFnPtr write_callback_body)


test_ffi : IO ()
test_ffi = do
  s <- foreign FFI_C "foo" (String -> CFnPtr ( String -> () ) -> IO String) "hello" (MkCFnPtr test_ffi_callback)

  putStrLn $ "string returned is: " ++ s

partial get_access_token : IO String
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

  config <- Effects.run $ readYAMLConfig "secrets.yaml"
  putStrLn "Config is:"
  printLn config

  config_map <- shred_config config

  putStrLn "config shredded to map:"
  printLn config_map


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

{-
  config_map <- case config of
    Right yamlDoc =>
      do putStrLn "Got YAMLNode..."
         case yamlDoc of
           YAMLDoc _ yamlMap =>
             do putStrLn " ... which is a YamlDoc"
                case yamlMap of
                  YAMLMap map => pure map
                -- yamlMap is YAMLMap (List (YAMLNode, YAMLNode))
-}
{-
    Left configError =>
      do putStrLn "config error"
         printLn configError
         pure []
-}
           



  -- now init an easy session, giving an easy handle.

  putStrLn "Initialising easy session"
  easy_handle <- foreign FFI_C "curl_easy_init" (IO (Ptr))
  -- TODO: check easy_handle for non-null

  ret <- curlEasySetopt easy_handle CurlOptionUrl "https://www.reddit.com/api/v1/access_token"

  ret <- curlEasySetopt easy_handle CurlOptionUserAgent "idris-todaybot DEVELOPMENT/TESTING by u/benclifford"

  putStrLn "set user agent result code:"
  printLn ret

  ret <- curlEasySetopt easy_handle CurlOptionUserPwd (app_id ++ ":" ++ app_token)

  ret <- curlEasySetopt easy_handle CurlOptionCopyPostFields ("grant_type=password&username=" ++ username ++ "&password=" ++ password)

  ret <- curlEasySetopt easy_handle CurlOptionVerbose 1


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

  -- TODO: replace 16 with sizeof a Ptr. but 16 should be big
  -- enough for now.
  content_buf_ptr <- alloc_bytes 16
  poke_ptr content_buf_ptr null_pointer
  ret <- curlEasySetopt easy_handle CurlOptionWriteData content_buf_ptr

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

  ret <- foreign FFI_C "curl_easy_perform" (Ptr -> IO Int) easy_handle
  -- TODO: assert ret == 0//CURLE_OK

  putStrLn "easy_perform return code:"
  printLn ret

  -- at this point, content_buf_ptr should be a **content
  -- reference that we can somehow cast into a string
  -- and then parse as JSON. and then leave to the winds
  -- (or rather, some kind of cleardown point that we believe
  -- it is safe to unalloc?) - regions/linear types?


  foreign FFI_C "dump_buffer" (Ptr -> IO ()) content_buf_ptr

  -- QUESTION/DISCUSSION: what's the right/best way to get an
  -- idris string out of this buffer? (there might be encoding
  -- issues?)
  response_body <- foreign FFI_C "cast_to_string_helper" (Ptr -> IO String) content_buf_ptr

  putStrLn "idris-side: buffer string is: "
  putStrLn response_body

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

get_hot_posts : String -> IO String
get_hot_posts access_token = do
  -- now we can make calls to oauth.reddit.com using the access token

  -- getHotPosts using libcurl.

  easy_handle2 <- foreign FFI_C "curl_easy_init" (IO (Ptr))

  -- TODO: better abstractions for this slist? Can we do it functionally
  -- using unsafePerformIO? and using a better pointer type rather than
  -- Ptr.

  slist <- foreign FFI_C "curl_slist_append" (Ptr -> String -> IO Ptr) null_pointer ("Authorization: " ++ "bearer " ++ access_token)

  -- TODO: factor this for calling on any http request
  ret <- curlEasySetopt easy_handle2 CurlOptionWriteFunction write_callback
  content_buf_ptr <- alloc_bytes 16
  poke_ptr content_buf_ptr null_pointer
  ret <- curlEasySetopt easy_handle2 CurlOptionWriteData content_buf_ptr


  -- TODO: factor into "set todaybot useragent header"
  ret <- curlEasySetopt easy_handle2 CurlOptionUserAgent "idris-todaybot DEVELOPMENT/TESTING by u/benclifford"

  ret <- curlEasySetopt easy_handle2 CurlOptionHttpHeader slist

  ret <- curlEasySetopt easy_handle2 CurlOptionUrl "https://oauth.reddit.com/r/LondonSocialClub/hot?limit=30"
  -- TODO: check ret

  putStrLn "Performing easy session (2)"

  ret <- foreign FFI_C "curl_easy_perform" (Ptr -> IO Int) easy_handle2
  -- TODO: assert ret == 0//CURLE_OK

  putStrLn "easy_perform return code:"
  printLn ret

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

  foreign FFI_C "dump_buffer" (Ptr -> IO ()) content_buf_ptr

  response_body <- foreign FFI_C "cast_to_string_helper" (Ptr -> IO String) content_buf_ptr

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


-- is this in std library? should it be?
-- (waffle: maybeHead is a natural transformation but
-- not a monad homomorphism...)
-- Goes alongside eitherToMaybe as a morphism and
-- maybe should be called listToMaybe to line up
-- with that?
maybeHead : List a -> Maybe a
maybeHead [] = Nothing
maybeHead (x::xs) = Just x


-- QUESTION/DISCUSSION:
-- This got factored into its own function because I found
-- that it was easier to diagnose type problems by having
-- more explicit type signatures on functions (rather than
-- letting them be inferred inside a big main and getting
-- an error right at the start of main)
get_first_hot_post : JsonValue -> Maybe JsonValue
get_first_hot_post ps = do
  array <- arrayFromJSON ps
  maybeHead array

partial main : IO ()
main = do
  putStrLn "idris ffi test start"

  test_ffi

  putStrLn $ "calling global init for curl"
  -- TODO: send it proper init code not 3 (extract from lib...)
  ret <- foreign FFI_C "curl_global_init" (Int -> IO Int) 3
  printLn ret
  -- TODO: check ret == 0
  putStrLn $ "called global init for curl"

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

  let p = (maybe_hot_post_Listing >>= get_first_hot_post)

  putStrLn "First hot post:"
  printLn p

  -- now extract the heading from this:
  -- in /data/

  let posttitle = do
        post <- p
        postdata <- getkey "data" post
        js <- getkey "title" postdata
        stringFromJSON js

  let postflair = do
        post <- p
        postdata <- getkey "data" post
        text <- getkey "link_flair_text" postdata
        css <- getkey "link_flair_css_class" postdata
        pure (text,css)

  putStrLn "Post title:"
  printLn posttitle
  putStrLn "Post flair:"
  printLn postflair

  let postdate = posttitle >>= titleToDate
  putStrLn "Post date:"
  printLn postdate

  -- now, we need a date parser to parse out the
  -- date from the posttitle. The Haskell todaybot
  -- has a parsec parser for this that hopefully
  -- will port to lightyear easily. (and I just got
  -- where the name lightyear comes from)

  putStrLn "Shutting down libcurl"
  ret <- foreign FFI_C "curl_global_cleanup" (IO ())
  putStrLn "idris ffi test end"


