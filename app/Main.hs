module Main where

import           TwitchParser         (extractViewerCount)

import qualified Data.ByteString.Lazy as B (getContents)

main :: IO ()
main = do
  json <- B.getContents
  putStrLn $ humanViewerCount $ extractViewerCount json

humanViewerCount :: Maybe Int -> String
humanViewerCount (Just i)
  | i > 1     = (show i) ++ " spectateurs"
  | i == 1    = (show i) ++ " spectateur"
  | otherwise = offline
humanViewerCount Nothing = offline

offline :: String
offline = "offline"
