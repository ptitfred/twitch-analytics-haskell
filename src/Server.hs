{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server
  ( application
  ) where

import           Database               (Connection, Pool, Video, listVideos)
import           Templating             (page)

import           Control.Monad.IO.Class (liftIO)
import           Data.Proxy
import           Servant.API
import           Servant.HTML.Blaze
import           Servant.Server
import           Text.Blaze             (ToMarkup (..))

newtype Index = Index [Video]

instance ToMarkup Index where
  toMarkup (Index videos) = page videos

type API = Get '[HTML] Index
      :<|> "index.html" :> Get '[HTML] Index

server :: Pool Connection -> Server API
server pool = serveIndex pool :<|> serveIndex pool

serveIndex :: Pool Connection -> Handler Index
serveIndex pool = Index <$> liftIO (listVideos pool)

api :: Proxy API
api = Proxy

application :: Pool Connection -> Application
application pool = serve api (server pool)
