%include C "ffitest.h"
%link C "ffitest.o"

%include C "curl/curl.h"

callback : String -> ()
callback s = unsafePerformIO $ do
  putStrLn $ "CALLBACK!!! string=" ++ s

data CurlOption : Type where
  CurlOptionUrl : CurlOption

curlOptionToFFI : CurlOption -> Int
curlOptionToFFI CurlOptionUrl = 10002

curlOptionType : CurlOption -> Type
curlOptionType CurlOptionUrl = String


main : IO ()
main = do
  putStrLn "idris ffi test start"

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


  --   curl_easy_setopt(easy_handle, CURLOPT_URL, "https://www.reddit.com/api/v1/access_token");
  -- how to get at CURLOPT values in Idris? If I'm doing the dependent
  -- type thing, then they maybe need to be explicitly listed?

  ret <- foreign FFI_C "curl_easy_setopt" (Ptr -> Int -> String -> IO Int) easy_handle (curlOptionToFFI CurlOptionUrl) "https://www.reddit.com/api/v1/access_token"

  putStrLn "Performing easy session"
  ret <- foreign FFI_C "curl_easy_perform" (Ptr -> IO Int) easy_handle
  -- TODO: assert ret == 0//CURLE_OK

  putStrLn "easy_perform return code:"
  printLn ret

  putStrLn "Shutting down libcurl"
  ret <- foreign FFI_C "curl_global_cleanup" (IO ())
  putStrLn "idris ffi test end"

