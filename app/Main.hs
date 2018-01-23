module Main where

import           TwitchAPI          (fetchViewerCount)

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  forever $ do
    process (getOutputPath args)
    putStrLn "."
    wait 10

wait :: Int -> IO ()
wait seconds = threadDelay (toMicroseconds seconds)

toMicroseconds :: Int -> Int
toMicroseconds seconds = seconds * 1000 * 1000

process :: Maybe FilePath -> IO ()
process = maybe missingArgument updateViewerCount
  where
    missingArgument = putStrLn "Missing argument: output path filename"

updateViewerCount :: FilePath -> IO ()
updateViewerCount path = do
  viewerCount <- fetchViewerCount "ptit_fred"
  writeFile path (humanViewerCount viewerCount)

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
