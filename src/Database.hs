{-# LANGUAGE OverloadedStrings #-}

module Database
  ( Video(..)
  , URL
  , insertVideo
  , listVideos

  , Tag(..)
  , insertTagsForVideo
  , listTags
  , listTagsForVideos

  , TaggedVideo
  , listTaggedVideos

  , newPool
  , Pool
  , Connection
  ) where

import           Database.Connection

import           Control.Exception.Base             (catch)
import           Data.Pool                          (Pool, withResource)
import           Data.Text                          (Text)
import           Data.Time.Calendar                 (Day)
import           Database.PostgreSQL.Simple         (Connection, Only (..),
                                                     ResultError, SqlError,
                                                     execute, executeMany,
                                                     query, query_)
import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)
import           Database.PostgreSQL.Simple.ToField (ToField (..), toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow (..))

type URL = String

type TaggedVideo = (Video, [Tag])

data Video =
  Video { url         :: URL
        , youtubeId   :: String
        , title       :: Text
        , description :: Text
        , streamDate  :: Day
        } deriving Show

instance ToRow Video where
  toRow (Video u yid t d sd) = [toField u, toField yid, toField t, toField d, toField sd]

instance FromRow Video where
  fromRow =
    Video <$> field <*> field <*> field <*> field <*> field

insertVideo :: Pool Connection -> Video -> IO Bool
insertVideo pool video = insertVideoUnsafe pool video `catch` handleError
  where
    handleError :: SqlError -> IO Bool
    handleError _ = pure False

insertVideoUnsafe :: Pool Connection -> Video -> IO Bool
insertVideoUnsafe pool video = isSuccess <$> withResource pool insert
  where
    isSuccess rows = rows == 1
    insert connection = execute connection insertQuery video
    insertQuery = "INSERT INTO videos ( url, youtube_id, title, description, stream_date ) VALUES (?, ?, ?, ?, ?)"

listVideos :: Pool Connection -> IO [Video]
listVideos = catchSelectError . listVideosUnsafe

listVideosUnsafe :: Pool Connection -> IO [Video]
listVideosUnsafe pool = withResource pool selectVideos
  where
    selectVideos connection = query_ connection selectQuery
    selectQuery = "SELECT url, youtube_id, title, description, stream_date FROM videos"

newtype Tag = Tag { getTag :: Text } deriving Show

instance FromRow Tag where
  fromRow = Tag <$> field

insertTagsForVideo :: Pool Connection -> Video -> [Tag] -> IO Bool
insertTagsForVideo pool video tags = insertTagsForVideoUnsafe pool video tags `catch` handleError
  where
    handleError :: SqlError -> IO Bool
    handleError _ = pure False

insertTagsForVideoUnsafe :: Pool Connection -> Video -> [Tag] -> IO Bool
insertTagsForVideoUnsafe pool video tags = isSuccess <$> withResource pool insert
  where
    isSuccess rows = toInteger rows == toInteger (length tags)
    insert connection = executeMany connection insertQuery parameters
    parameters :: [(Text, URL)]
    parameters = map mkParameter tags
    mkParameter :: Tag -> (Text, URL)
    mkParameter tag = (getTag tag, url video)
    insertQuery = "INSERT INTO tags ( tag, video_url ) VALUES (?, ?)"

listTags :: Pool Connection -> IO [Tag]
listTags = catchSelectError . listTagsUnsafe

listTagsUnsafe :: Pool Connection -> IO [Tag]
listTagsUnsafe pool = withResource pool selectTags
  where
    selectTags connection = query_ connection selectQuery
    selectQuery = "SELECT DISTINCT tag FROM tags"

listTagsForVideos :: Pool Connection -> Video -> IO [Tag]
listTagsForVideos pool video = catchSelectError (listTagsForVideosUnsafe pool video)

listTagsForVideosUnsafe :: Pool Connection -> Video -> IO [Tag]
listTagsForVideosUnsafe pool video = withResource pool selectTags
  where
    selectTags connection = query connection selectQuery (Only (url video))
    selectQuery = "SELECT DISTINCT tag FROM tags WHERE video_url = ?"

listTaggedVideos :: Pool Connection -> IO [TaggedVideo]
listTaggedVideos pool = listVideos pool >>= mapM (fetchTaggedVideo pool)

fetchTaggedVideo :: Pool Connection -> Video -> IO TaggedVideo
fetchTaggedVideo pool v = do
  tags <- listTagsForVideos pool v
  pure (v, tags)

catchSelectError :: IO [a] -> IO [a]
catchSelectError f = f `catch` handleSqlError `catch` handleResultError

handleSqlError :: SqlError -> IO [a]
handleSqlError _ = pure []

handleResultError :: ResultError -> IO [a]
handleResultError _ = pure []
