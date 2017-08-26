module DNAnts
  ( runApp
  ) where

import Data.DNAnts.AppSettings (AppSettings(AppSettings))

runApp :: String -> AppSettings -> IO ()
runApp title settings = do
  print $ "running app: " ++ title
  print $ show settings