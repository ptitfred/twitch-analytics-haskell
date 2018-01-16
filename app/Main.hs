module Main where

import           TwitchAPI          (fetchViewerCount)

import           Control.Concurrent (threadDelay)
import           System.Environment (getArgs)

main :: IO ()
main = watch 10 process

watch :: Int -> IO () -> IO ()
watch seconds action = do
  action
  putStrLn "."
  threadDelay (toMicroseconds seconds)
  watch seconds action

toMicroseconds :: Int -> Int
toMicroseconds seconds = seconds * 1000 * 1000

process :: IO ()
process = do
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
