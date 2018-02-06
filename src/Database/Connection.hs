module Database.Connection
  ( newPool
  ) where

import           Data.Pool                          (Pool, createPool)
import           Database.PostgreSQL.Simple         (ConnectInfo (..),
                                                     Connection, close, connect)
import           System.Environment                 (getEnv)

newPool :: IO (Pool Connection)
newPool = createPool getConnection close subPools timeout maxConnections
  where
    subPools = 1
    timeout = 30
    maxConnections = 5

getConnection :: IO Connection
getConnection = connect =<< getConnectInfo

getConnectInfo :: IO ConnectInfo
getConnectInfo =
  ConnectInfo <$> getEnv "PG_HOST"
              <*> (read <$> getEnv "PG_PORT")
              <*> getEnv "PG_USER"
              <*> getEnv "PG_PASS"
              <*> getEnv "PG_DBNAME"
