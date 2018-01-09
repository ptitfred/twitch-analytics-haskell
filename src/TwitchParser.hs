{-# LANGUAGE OverloadedStrings #-}

module TwitchParser
    ( fetchViewerCount
    , interpretMetadatas
    ) where

import           Types

import           TwitchAPI (fetchMetadatas)

fetchViewerCount :: UserLogin -> IO (Maybe Int)
fetchViewerCount userLogin = interpretMetadatas <$> fetchMetadatas userLogin

interpretMetadatas :: Either String Metadatas -> Maybe Int
interpretMetadatas (Left   _)     = Nothing
interpretMetadatas (Right result) = extract result

extract :: Metadatas -> Maybe Int
extract (Metadatas [])    = Nothing
extract (Metadatas (m:_)) = Just (viewerCount m)
