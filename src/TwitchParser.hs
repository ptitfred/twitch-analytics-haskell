module TwitchParser
    ( extractViewerCount
    ) where

import           Data.ByteString (ByteString)

extractViewerCount :: ByteString -> Int
extractViewerCount = const 0
