{-# LANGUAGE OverloadedStrings #-}

module TwitchParser
    ( extractViewerCount
    ) where

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)

extractViewerCount :: ByteString -> Maybe Int
extractViewerCount = maybeViewerCount . maybeFirst . maybeData . decode

maybeData :: Maybe Metadatas -> Maybe [Metadata]
maybeData = fmap metadatas

maybeFirst :: Maybe [a] -> Maybe a
maybeFirst (Just (m:_)) = Just m
maybeFirst _            = Nothing

maybeViewerCount :: Maybe Metadata -> Maybe Int
maybeViewerCount = fmap viewerCount

data Metadatas = Metadatas { metadatas :: [Metadata] }

instance FromJSON Metadatas where
  parseJSON (Object o) = Metadatas <$> o .: "data"
  parseJSON invalid    = fail "unexpected"

data Metadata = Metadata { viewerCount :: Int }

instance FromJSON Metadata where
  parseJSON (Object o) = Metadata <$> o .: "viewer_count"
  parseJSON invalid    = fail "unexpected"
