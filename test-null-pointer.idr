import Todaybot.Ptr

-- this tests that null pointer can be built without needing
-- other imports

main : IO ()
main = do
  let x = null_pointer
  printLn $ show $ x == null_pointer -- this is needed to force the
                                     -- code for 'x' to be compiled because
                                     -- it seems if it is unused, it gets
                                     -- elided.
  putStrLn "null pointer done"

