{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Data.Text                          (Text)
import           Database.PostgreSQL.Simple         (ConnectInfo (..),
                                                     Connection, close, connect,
                                                     execute, query_)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField (ToField (..), toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))
import           System.Environment                 (getEnv)

type URL = String

data Video =
  Video { url         :: URL
        , title       :: Text
        , description :: Text
        } deriving Show

instance ToRow Video where
  toRow (Video u t d) = [toField u, toField t,  toField d]

instance FromRow Video where
  fromRow = Video <$> field <*> field <*> field

insertVideo :: Video -> IO Bool
insertVideo video = do
  connection <- getConnection
  affectedRows <- execute connection query video
  close connection
  pure (affectedRows == 1)
   where
    query = "INSERT INTO videos ( url, title, description ) VALUES (?, ?, ?)"

getConnection :: IO Connection
getConnection = do
  connectInfos <- getConnectInfos
  connect connectInfos

getConnectInfos :: IO ConnectInfo
getConnectInfos = do
  host <- getEnv "PG_HOST"
  port <- getEnv "PG_PORT"
  user <- getEnv "PG_USER"
  pass <- getEnv "PG_PASS"
  dbname <- getEnv "PG_DBNAME"
  pure $ ConnectInfo host (read port) user pass dbname

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
