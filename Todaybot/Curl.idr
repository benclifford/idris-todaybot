module Todaybot.Curl

import Todaybot.Ptr

%access public export

%lib C "curl"
%include C "curl/curl.h"

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
curlEasySetoptString easy_handle opt param =
  foreign FFI_C "curl_easy_setopt" (Ptr -> Int -> String -> IO Int) easy_handle (curlOptionToFFI opt) param

total curlEasySetoptLong : EasyHandle -> (opt : CurlOption) -> Int -> IO Int
curlEasySetoptLong easy_handle opt param =
  foreign FFI_C "curl_easy_setopt" (Ptr -> Int -> Int -> IO Int) easy_handle (curlOptionToFFI opt) param

total curlEasySetoptPtr : EasyHandle -> (opt : CurlOption) -> Ptr -> IO Int
curlEasySetoptPtr easy_handle opt param =
  foreign FFI_C "curl_easy_setopt" (Ptr -> Int -> Ptr -> IO Int) easy_handle (curlOptionToFFI opt) param


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
  checkPointerNotNull longterm_buf

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


curlEasyInit : IO Ptr
curlEasyInit = foreign FFI_C "curl_easy_init" (IO (Ptr))

curlEasyCleanup : Ptr -> IO ()
curlEasyCleanup handle = foreign FFI_C "curl_easy_cleanup" (Ptr -> IO ()) handle

curlGlobalInit : IO Int
curlGlobalInit = foreign FFI_C "curl_global_init" (Int -> IO Int) 3

curlGlobalCleanup : IO ()
curlGlobalCleanup = foreign FFI_C "curl_global_cleanup" (IO ())

curlEasyPerform : Ptr -> IO Int
curlEasyPerform handle = foreign FFI_C "curl_easy_perform" (Ptr -> IO Int) handle

curlSListAppend : Ptr -> String -> IO Ptr
curlSListAppend list str = foreign FFI_C "curl_slist_append" (Ptr -> String -> IO Ptr) list str

curlSListFreeAll : Ptr -> IO ()
curlSListFreeAll list = foreign FFI_C "curl_slist_free_all" (Ptr -> IO ()) list 

-- QUESTION/DISCUSSION:
-- This is a fairly ugly way of causing the program to crash on a curl
-- error rather than continuing silently.
-- It should turn into something like effectful exceptions?
checkCurlRet : Int -> IO ()
checkCurlRet 0 = pure ()
checkCurlRet _ = ?curl_return_code_nonzero
