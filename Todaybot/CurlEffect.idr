module CurlEffect

import Todaybot.Curl

import Effects

import Todaybot.Ptr

%access public export

-- this is very boilerplatey...
-- defining not just functions to perform
-- the work, but wrapper functions to expose with the
-- right signature, and a data type representation of
-- those function calls...
data Curl : Effect where
  CurlEasyInit : sig Curl EasyHandle
  CurlEasySetopt : EasyHandle -> (opt : CurlOption) -> curlOptionType opt -> sig Curl Int
  CurlEasyPerform : EasyHandle -> sig Curl Int
  CurlEasyCleanup : EasyHandle -> sig Curl ()
  CurlSListFreeAll : Ptr -> sig Curl ()
  CurlSListAppend : Ptr -> String -> sig Curl Ptr


CURL : EFFECT
CURL = MkEff () Curl

curlEasyInit : Eff Ptr [CURL]
curlEasyInit = call CurlEasyInit


curlEasySetopt : EasyHandle -> (opt : CurlOption) -> curlOptionType opt -> Eff Int [CURL]
curlEasySetopt h opt val = call $ CurlEasySetopt h opt val

curlEasyPerform : EasyHandle -> Eff Int [CURL]
curlEasyPerform h = call $ CurlEasyPerform h

curlEasyCleanup : EasyHandle -> Eff () [CURL]
curlEasyCleanup h = call $ CurlEasyCleanup h

curlSListFreeAll : Ptr -> Eff () [CURL]
curlSListFreeAll l = call $ CurlSListFreeAll l

curlSListAppend : Ptr -> String -> Eff Ptr [CURL]
curlSListAppend l s = call $ CurlSListAppend l s

Handler Curl IO where

  handle () CurlEasyInit k = do
    h <- Todaybot.Curl.curlEasyInit
    k h ()

  handle () (CurlEasySetopt h opt val) k = do
    ret <- Todaybot.Curl.curlEasySetopt h opt val
    k ret ()

  handle () (CurlEasyPerform h) k = do
    ret <- Todaybot.Curl.curlEasyPerform h
    k ret ()

  handle () (CurlEasyCleanup h) k = do
    Todaybot.Curl.curlEasyCleanup h
    k () ()

  handle () (CurlSListFreeAll l) k = do
    Todaybot.Curl.curlSListFreeAll l
    k () ()

  handle () (CurlSListAppend l s) k = do
    ptr <- Todaybot.Curl.curlSListAppend l s
    k ptr ()
