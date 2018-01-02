{-# LANGUAGE OverloadedStrings #-}

module TwitchParser
    ( extractViewerCount
    ) where

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)

extractViewerCount :: ByteString -> Int
extractViewerCount json =
  case parser json of
    Just ms -> checkViewerCount (metadatas ms)
    Nothing -> 0

checkViewerCount :: [Metadata] -> Int
checkViewerCount [] = 0
checkViewerCount ms = viewerCount (head ms)

parser :: ByteString -> Maybe Metadatas
parser json = decode json

data Metadatas = Metadatas { metadatas :: [Metadata] }

instance FromJSON Metadatas where
  parseJSON (Object o) = Metadatas <$> o .: "data"
  parseJSON invalid = fail "unexpected"

data Metadata =
  Metadata
    { viewerCount :: Int
    }

instance FromJSON Metadata where
  parseJSON (Object o) = Metadata <$> o .: "viewer_count"
  parseJSON invalid = fail "unexpected"
