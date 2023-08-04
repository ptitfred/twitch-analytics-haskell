{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Loader where

import           Database           (Connection, Pool, Video (..), insertVideo,
                                     newPool)

import           Data.Monoid        ((<>))
import           Data.Time.Calendar (fromGregorian)

examples :: [Video]
examples =
  [ example1
  , example2
  , example3
  ]

main :: IO ()
main = do
  pool <- newPool
  results <- mapM (insertVideoAndDescribe pool) examples
  if and results
  then putStrLn "Every video has been inserted."
  else putStrLn "Some videos have failed inserting."

insertVideoAndDescribe :: Pool Connection -> Video -> IO Bool
insertVideoAndDescribe pool video = do
  result <- insertVideo pool video
  if result
  then putStrLn ("Video " <> youtubeId video <> " inserted")
  else putStrLn ("Video " <> youtubeId video <> " failed")
  pure result

example1 :: Video
example1 =
  let youtubeId = "fqSoq7SOo0Y"
      url = mkUrl youtubeId
      title = "Livecoding Haskell pour débutant #4"
      description = "Source du projet : https://github.com/ptitfred/twitch-an..."
      streamDate = fromGregorian 2018 1 23
  in Video{..}

example2 :: Video
example2 =
  let youtubeId = "lL2M8JVhhpU"
      url = mkUrl youtubeId
      title = "Livecoding Haskell pour débutant #3"
      description = "Source du projet : https://github.com/ptitfred/twitch-an..."
      streamDate = fromGregorian 2018 1 16
  in Video{..}

example3 :: Video
example3 =
  let youtubeId = "G9ta5yUq9cg"
      url = mkUrl youtubeId
      title = "Livecoding Haskell pour débutant #2"
      description = "Source du projet : https://github.com/ptitfred/twitch-an..."
      streamDate = fromGregorian 2018 1 9
  in Video{..}

mkUrl :: String -> String
mkUrl yid = "https://www.youtube.com/watch?v=" <> yid
