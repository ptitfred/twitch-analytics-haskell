{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module TwitchAPI
  ( fetchViewerCount
  ) where

import           TwitchParser (interpretMetadatas)
import           Types

import           Data.Proxy              (Proxy (..))
import           Servant.API
import           Servant.Client          (BaseUrl (..), ClientEnv (..), ClientM,
                                          Scheme (..), ServantError, client,
                                          runClientM)

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           System.Environment      (getEnv)

type API = "streams" :> QueryParam "user_login" UserLogin
                     :> Header     "Client-ID"  ClientID
                     :> Get '[JSON] Metadatas

api :: Proxy API
api = Proxy

fetchMetadatasClient :: Maybe UserLogin -> Maybe ClientID -> ClientM Metadatas
fetchMetadatasClient = client api

fetchMetadatas :: UserLogin -> IO (Either String Metadatas)
fetchMetadatas userLogin = do
  clientId <- getEnv "CLIENT_ID"
  manager <- newManager tlsManagerSettings
  let fetchClient = fetchMetadatasClient (Just userLogin) (Just clientId)
      baseUrl = BaseUrl { baseUrlScheme = Https
                        , baseUrlHost   = "api.twitch.tv"
                        , baseUrlPort   = 443
                        , baseUrlPath   = "/helix"
                        }
      clientEnv = ClientEnv manager baseUrl
  result <- runClientM fetchClient clientEnv
  pure (convertError result)

fetchViewerCount :: UserLogin -> IO (Maybe Int)
fetchViewerCount userLogin = interpretMetadatas <$> fetchMetadatas userLogin

convertError :: Either ServantError Metadatas -> Either String Metadatas
convertError (Left err)     = Left (serializeError err)
convertError (Right result) = Right result

serializeError :: ServantError -> String
serializeError = show
