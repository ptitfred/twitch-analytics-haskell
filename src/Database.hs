{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (ConnectInfo (..), Connection,
                                             close, connect, execute, query_)

import           System.Environment         (getEnv)

type URL = String

data Video =
  Video { url         :: URL
        , title       :: Text
        , description :: Text
        } deriving Show

insertVideo :: Video -> IO Bool
insertVideo video = do
  connection <- getConnection
  affectedRows <- execute connection query tuple
  close connection
  pure (affectedRows == 1)
   where
    tuple = (url video, title video, description video)
    query = "INSERT INTO videos ( url, title, description ) VALUES (?, ?, ?)"

getConnection :: IO Connection
getConnection = connect =<< getConnectInfo

getConnectInfo :: IO ConnectInfo
getConnectInfo =
  ConnectInfo <$> getEnv "PG_HOST"
              <*> (read <$> getEnv "PG_PORT")
              <*> getEnv "PG_USER"
              <*> getEnv "PG_PASS"
              <*> getEnv "PG_DBNAME"

listVideos :: IO [Video]
listVideos = do
  connection <- getConnection
  records <- query_ connection selectQuery
  close connection
  pure $ toVideos records
  where
    selectQuery = "SELECT url, title, description FROM videos"
    toVideo :: (URL, Text, Text) -> Video
    toVideo (url', title', description') = Video url' title' description'
    toVideos :: [(URL, Text, Text)] -> [Video]
    toVideos records = map toVideo records
