{-# LANGUAGE OverloadedStrings #-}

module TwitchParser
    ( interpretMetadatas
    ) where

import           Types

interpretMetadatas :: Either String Metadatas -> Maybe Int
interpretMetadatas (Left   _)     = Nothing
interpretMetadatas (Right result) = extract result

extract :: Metadatas -> Maybe Int
extract (Metadatas [])    = Nothing
extract (Metadatas (m:_)) = Just (viewerCount m)
