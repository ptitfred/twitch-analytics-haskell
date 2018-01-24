{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Data.Text                          (Text)
import           Database.PostgreSQL.Simple         (ConnectInfo (..),
                                                     Connection, close, connect,
                                                     execute, query_, defaultConnectInfo)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField (ToField (..), toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))
import           System.Environment                 (lookupEnv)

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
getConnection = connect =<< getConnectInfo

getConnectInfo :: IO ConnectInfo
getConnectInfo =
  ConnectInfo <$> lookupEnvOrDefault connectHost "PG_HOST"
              <*> (read <$> lookupEnvOrDefault (show . connectPort) "PG_PORT")
              <*> lookupEnvOrDefault connectUser "PG_USER"
              <*> lookupEnvOrDefault connectPassword "PG_PASS"
              <*> lookupEnvOrDefault connectDatabase "PG_DBNAME"
  where
    lookupEnvOrDefault defaultAccessor env =
      maybe (defaultAccessor defaultConnectInfo) id <$> lookupEnv env

listVideos :: IO [Video]
listVideos = do
  connection <- getConnection
  records <- query_ connection selectQuery
  close connection
  pure records
  where
    selectQuery = "SELECT url, title, description FROM videos"
