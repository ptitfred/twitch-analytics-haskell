module Main where

import           TwitchAPI          (fetchViewerCount)

import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case getOutputPath args of
    Just path -> do
                   viewerCount <- fetchViewerCount "ptit_fred"
                   writeFile path (humanViewerCount viewerCount)
    Nothing   -> putStrLn "Missing argument: output path filename"

getOutputPath :: [String] -> Maybe FilePath
getOutputPath (path : _) = Just path
getOutputPath []         = Nothing

humanViewerCount :: Maybe Int -> String
humanViewerCount (Just i)
  | i > 1     = (show i) ++ " spectateurs"
  | i == 1    = (show i) ++ " spectateur"
  | otherwise = offline
humanViewerCount Nothing = offline

offline :: String
offline = "offline"
