
-- -p config
import Config.YAML

-- -p effects
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

%lib C "curl"
%include C "ffitest.h"
%link C "ffitest.o"

%include C "curl/curl.h"

callback : String -> ()
callback s = unsafePerformIO $ do
  putStrLn $ "CALLBACK!!! string=" ++ s

data CurlOption : Type where
  CurlOptionUrl : CurlOption
  CurlOptionUserPwd : CurlOption
  CurlOptionCopyPostFields : CurlOption
  CurlOptionUserAgent : CurlOption
  CurlOptionVerbose : CurlOption
  CurlOptionWriteFunction : CurlOption

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

total curlOptionType : CurlOption -> Type
curlOptionType CurlOptionUrl = String
curlOptionType CurlOptionUserAgent = String
curlOptionType CurlOptionUserPwd = String
curlOptionType CurlOptionCopyPostFields = String
curlOptionType CurlOptionVerbose = Int
curlOptionType CurlOptionWriteFunction = Ptr

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
  CurlOptionUrl => curlEasySetoptString easy_handle opt param
  CurlOptionUserAgent => curlEasySetoptString easy_handle opt param
  CurlOptionUserPwd => curlEasySetoptString easy_handle opt param
  CurlOptionCopyPostFields => curlEasySetoptString easy_handle opt param
  CurlOptionVerbose => curlEasySetoptLong easy_handle opt param
  CurlOptionWriteFunction => curlEasySetoptPtr easy_handle opt param

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


write_callback_body : Ptr -> Int -> Int -> Ptr -> Int
write_callback_body curldata s1 s2 userdata = unsafePerformIO $ do
  putStrLn "In write_callback_body"
  pure (s1 * s2)

write_callback : Ptr
write_callback = unsafePerformIO $
  foreign FFI_C "%wrapper" (CFnPtr (Ptr -> Int -> Int -> Ptr -> Int) -> IO Ptr) (MkCFnPtr write_callback_body)

partial main : IO ()
main = do
  putStrLn "idris ffi test start"


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

  config <- run $ readYAMLConfig "secrets.yaml"
  putStrLn "Config is:"
  printLn config

  config_map <- shred_config config

  putStrLn "config shredded to map:"
  printLn config_map


-- QUESTION/DISCUSSION: using "Just username" in this let binding
-- causes idris command to run for at least 4 minutes (possibly
-- forever?). but fromJust form takes about 10s to compile.
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
           


  s <- foreign FFI_C "foo" (String -> CFnPtr ( String -> () ) -> IO String) "hello" (MkCFnPtr callback)

  putStrLn $ "string returned is: " ++ s

  putStrLn $ "calling global init for curl"
  -- TODO: send it proper init code not 3 (extract from lib...)
  ret <- foreign FFI_C "curl_global_init" (Int -> IO Int) 3
  printLn ret
  -- TODO: check ret == 0
  putStrLn $ "called global init for curl"


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

  putStrLn "Shutting down libcurl"
  ret <- foreign FFI_C "curl_global_cleanup" (IO ())
  putStrLn "idris ffi test end"
