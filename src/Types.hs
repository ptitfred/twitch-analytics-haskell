{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson

data Metadatas = Metadatas { metadatas :: [Metadata] } deriving Show

instance FromJSON Metadatas where
  parseJSON (Object o) = Metadatas <$> o .: "data"
  parseJSON _          = fail "unexpected"

data Metadata = Metadata { viewerCount :: Int } deriving Show

instance FromJSON Metadata where
  parseJSON (Object o) = Metadata <$> o .: "viewer_count"
  parseJSON _          = fail "unexpected"

type UserLogin = String
type ClientID = String
