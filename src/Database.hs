{-# LANGUAGE OverloadedStrings #-}

module Database
  ( Video(..)
  , URL
  , insertVideo
  , listVideos
  , newPool
  ) where

import           Control.Exception.Base             (catch)
import           Data.Pool                          (Pool, createPool,
                                                     withResource)
import           Data.Text                          (Text)
import           Database.PostgreSQL.Simple         (ConnectInfo (..),
                                                     Connection, ResultError,
                                                     SqlError, close, connect,
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

insertVideo :: Pool Connection -> Video -> IO Bool
insertVideo pool video = insertVideoUnsafe pool video `catch` handleError
  where
    handleError :: SqlError -> IO Bool
    handleError _ = pure False

insertVideoUnsafe :: Pool Connection -> Video -> IO Bool
insertVideoUnsafe pool video = isSuccess <$> withResource pool insert
  where
    isSuccess rows = rows == 1
    insert connection = execute connection query video
    query = "INSERT INTO videos ( url, title, description ) VALUES (?, ?, ?)"

listVideos :: Pool Connection -> IO [Video]
listVideos pool = listVideosUnsafe pool `catch` handleSqlError `catch` handleResultError
  where
    handleSqlError :: SqlError -> IO [Video]
    handleSqlError _ = handleError
    handleResultError :: ResultError -> IO [Video]
    handleResultError _ = handleError
    handleError = pure []

listVideosUnsafe :: Pool Connection -> IO [Video]
listVideosUnsafe pool = withResource pool selectVideos
  where
    selectVideos connection = query_ connection selectQuery
    selectQuery = "SELECT url, title, description FROM videos"

newPool :: IO (Pool Connection)
newPool = createPool getConnection close 1 0.5 2

getConnection :: IO Connection
getConnection = connect =<< getConnectInfo

getConnectInfo :: IO ConnectInfo
getConnectInfo =
  ConnectInfo <$> getEnv "PG_HOST"
              <*> (read <$> getEnv "PG_PORT")
              <*> getEnv "PG_USER"
              <*> getEnv "PG_PASS"
              <*> getEnv "PG_DBNAME"
