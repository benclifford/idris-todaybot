
import Todaybot.JSON as TJ

-- QUESTION/DISCUSSION: this it to get
-- instance in scope for Show JsonValue.
-- Although JsonValue comes in from TJ,
-- apparently the Show instance does not.
-- Note that the show instance is not
-- total.
import Config.JSON

parseTest : String -> IO ()
parseTest s = do
  putStrLn $ "Testing parse of string: " ++ s
  printLn $ TJ.fromString s

main : IO ()
main = do
  putStrLn "test-todaybot-json-parser: starting"

  parseTest "{\"hi\":\"x\"}"
{-
  parseTest "{\"hi\":\"x\""
  parseTest "{ \"hi\":\"x\""
-}
  parseTest "{\""
  parseTest "{ \""
  parseTest "{\"hi\":\"x\"},"
  parseTest "[1,2,3]"

  putStrLn "test-todaybot-json-parser: done"
