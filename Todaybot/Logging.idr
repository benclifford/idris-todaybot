module Todaybot.Logging

import Effects
import Effect.StdIO

%access public export

-- QUESTION/DISCUSSION: I think it would be useful for
-- the parameters to these log functions to be Lazy as
-- sometimes they will be quite expensive to render.
--
-- This will give similar behaviour, executionwise, as
-- #ifdef-ing away debug statements, by not preparing
-- arguments that are then going to be ignored.

logError : String -> Eff () [STDIO]
logError s = putStrLn $ "ERROR: " ++ s

logWarn : String -> Eff () [STDIO]
logWarn s = putStrLn $ "WARN: " ++ s

logInfo : String -> Eff () [STDIO]
logInfo s = putStrLn $ "INFO: " ++ s

logDebug : String -> Eff () [STDIO]
logDebug s = putStrLn $ "DEBUG: " ++ s

