module DNAnts.Debug where

debugShow :: Show a => [Char] -> a -> IO ()
debugShow prefix =
  let prefix' = prefix ++ ": "
  in putStrLn . (prefix' ++) . show