module Main where

import           TwitchAPI          (fetchViewerCount)

import           System.Environment (getEnv)

main :: IO ()
main = do
  viewerCount <- fetchViewerCount "ptit_fred"
  home <- getEnv "HOME"
  writeFile (home ++ "/livecoding/viewer-count.txt") (humanViewerCount viewerCount)

humanViewerCount :: Maybe Int -> String
humanViewerCount (Just i)
  | i > 1     = (show i) ++ " spectateurs"
  | i == 1    = (show i) ++ " spectateur"
  | otherwise = offline
humanViewerCount Nothing = offline

offline :: String
offline = "offline"
