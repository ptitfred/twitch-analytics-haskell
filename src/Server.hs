{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Server
  ( application
  ) where

import           Database               (Connection, Pool, TaggedVideo,
                                         listTaggedVideos)
import           Templating             (page)

import           Control.Monad.IO.Class (liftIO)
import           Data.Proxy
import           Servant.API
import           Servant.HTML.Blaze
import           Servant.Server
import           Servant.Utils.StaticFiles
import           Text.Blaze             (ToMarkup (..))

newtype Index = Index [TaggedVideo]

instance ToMarkup Index where
  toMarkup (Index videos) = page videos

type API = Get '[HTML] Index
      :<|> "index.html" :> Get '[HTML] Index
      :<|> "public" :> Raw

server :: Pool Connection -> Server API
server pool = serveIndex pool
         :<|> serveIndex pool
         :<|> serveDirectory "public"

serveIndex :: Pool Connection -> Handler Index
serveIndex pool = Index <$> liftIO (listTaggedVideos pool)

api :: Proxy API
api = Proxy

application :: Pool Connection -> Application
application pool = serve api (server pool)
