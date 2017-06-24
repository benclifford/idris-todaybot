module CurlEffect

import Todaybot.Curl

import Effects

import Todaybot.Ptr

%access public export

-- QUESTION/DISCUSSION: lots of exploring about where I can put case
-- statenments, and what then can be cases of.
-- for example, case r of 0 / otherwise - in the "otherwise" path, I
--  can't tell that r != 0, which means I can't prove that the
-- result type is failure.
--
-- so then I tried matching on 'r == 0' / True / False, but that
-- didn't work either
--
-- driving me to try converting r from an Int to an Either Int ()
-- to turn that into a single failure constructor (Left) to match
-- on - removing some boolean blindness caused by the r == 0 test, I
-- hope (although I haven't tried at time of writing)



-- QUESTION/DISCUSSION: the present implementation is working towards
-- statically verifying if curl has been initialised or not.
-- A further development might be to track whether an easy handle
-- has been created/initialised, and how it should be properly closed.
-- With a single threaded use with only one easy handle at once, this
-- might be easily encoded as "uninitialised, globally initialised,
-- handle initialised, back to globally initialised, back to uninitialised"
-- but would change how EasyHandle gets passed around (to record it as
-- part of the state rather than as a variable that can go around
-- freely)

-- or maybe there should be a stateful effect CURL for the global
-- initialisation state, and a separate state CURLEASYHANDLE which
-- tracks an easy handle, with its own state, and the function
-- which creates that needs 'CURL CurlInitOK' ?
-- and anything which uses an easy handle needs both the appropriate
-- CURLEASYHANDLE and also 'CURL CurlInitOK' to check the user
-- hasn't de-initialised Curl globally.
-- SList handling might fit in there somehow too? Orthogonally
-- to easy handles...

globalRetToMaybe : Int -> Maybe Int
globalRetToMaybe 0 = Nothing -- no error
globalRetToMaybe r = Just r

data CurlNotInit : Type where
  CurlNotInitV : CurlNotInit

Default CurlNotInit where
  default = CurlNotInitV

-- there is no 'handle' structure at this stage
-- so we have a single value
data CurlInitOK : Type where
  CurlInitOKV : CurlInitOK

-- CurlNotInit : Type -- TODO: lose this
-- CurlNotInit = ()

-- this is very boilerplatey...
-- defining not just functions to perform
-- the work, but wrapper functions to expose with the
-- right signature, and a data type representation of
-- those function calls...
data Curl : Effect where
  CurlGlobalInit : sig Curl (Maybe Int) CurlNotInit (\ret => case ret of
                                               Nothing => CurlInitOK
                                               Just _ => CurlNotInit
                                            )
  CurlGlobalCleanup : sig Curl () CurlInitOK CurlNotInit
  CurlEasyInit : sig Curl EasyHandle CurlInitOK
  CurlEasySetopt : EasyHandle -> (opt : CurlOption) -> curlOptionType opt -> sig Curl Int CurlInitOK
  CurlEasyPerform : EasyHandle -> sig Curl Int CurlInitOK
  CurlEasyCleanup : EasyHandle -> sig Curl () CurlInitOK
  CurlSListFreeAll : Ptr -> sig Curl () CurlInitOK
  CurlSListAppend : Ptr -> String -> sig Curl Ptr CurlInitOK


CURL : Type -> EFFECT
CURL initState = MkEff initState Curl


-- QUESTION/DISCUSSION: this is a bit boilerplatey:
-- the effect I'm describing in this simple wrapper is the
-- same as the effect as described in 'data Curl'
curlGlobalInit : Eff (Maybe Int) [CURL CurlNotInit] (\ret => [CURL (case ret of
                                               Nothing => CurlInitOK
                                               Just _ => CurlNotInit
                                            )])
curlGlobalInit = do
  ret <- call CurlGlobalInit
  pureM ret

curlGlobalCleanup : Eff () [CURL CurlInitOK] [CURL CurlNotInit]
curlGlobalCleanup = call CurlGlobalCleanup

curlEasyInit : Eff Ptr [CURL CurlInitOK]
curlEasyInit = call CurlEasyInit


curlEasySetopt : EasyHandle -> (opt : CurlOption) -> curlOptionType opt -> Eff Int [CURL CurlInitOK]
curlEasySetopt h opt val = call $ CurlEasySetopt h opt val

curlEasyPerform : EasyHandle -> Eff Int [CURL CurlInitOK]
curlEasyPerform h = call $ CurlEasyPerform h

curlEasyCleanup : EasyHandle -> Eff () [CURL CurlInitOK]
curlEasyCleanup h = call $ CurlEasyCleanup h

curlSListFreeAll : Ptr -> Eff () [CURL CurlInitOK]
curlSListFreeAll l = call $ CurlSListFreeAll l

curlSListAppend : Ptr -> String -> Eff Ptr [CURL CurlInitOK]
curlSListAppend l s = call $ CurlSListAppend l s

Handler Curl IO where

  handle CurlNotInit CurlGlobalInit k = do
    r <- Todaybot.Curl.curlGlobalInit
    case globalRetToMaybe r of
      Nothing => k Nothing CurlInitOKV
      Just r => k (Just r) CurlNotInitV

  handle st CurlGlobalCleanup k = do
    Todaybot.Curl.curlGlobalCleanup
    k () CurlNotInitV

  handle st CurlEasyInit k = do
    h <- Todaybot.Curl.curlEasyInit
    k h st

  handle st (CurlEasySetopt h opt val) k = do
    ret <- Todaybot.Curl.curlEasySetopt h opt val
    k ret st

  handle st (CurlEasyPerform h) k = do
    ret <- Todaybot.Curl.curlEasyPerform h
    k ret st

  handle st (CurlEasyCleanup h) k = do
    Todaybot.Curl.curlEasyCleanup h
    k () st

  handle st (CurlSListFreeAll l) k = do
    Todaybot.Curl.curlSListFreeAll l
    k () st

  handle st (CurlSListAppend l s) k = do
    ptr <- Todaybot.Curl.curlSListAppend l s
    k ptr st
