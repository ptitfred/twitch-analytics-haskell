{-# LANGUAGE OverloadedStrings #-}

module Templating
  ( page
  , TaggedVideo
  ) where

import           Database                    as D (Tag (..), TaggedVideo,
                                                   Video (..))

import           Data.String                 (fromString)
import           Data.Text                   (Text)
import           Data.Time.Calendar          (showGregorian)
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as HA

page :: [TaggedVideo] -> Html
page videos = do
  docType
  html $ do
    pageHeader
    body $
      listVideos videos

pageHeader :: Html
pageHeader = do
  H.title "Livecoding Haskell pour débutant"
  meta ! charset "UTF-8"

listVideos :: [TaggedVideo] -> Html
listVideos []     = p $ toHtml noVideos
listVideos videos = ul $ mconcat (fmap listItem videos)

noVideos :: Text
noVideos = "No video found :-("

listItem :: TaggedVideo -> Html
listItem (v, tags) =
  li ! class_ "video" $ do
    H.div
      ! class_ "title"
      $ linkToVideo v
    videoDate v
    videoDescription v
    listTags tags

linkToVideo :: Video -> Html
linkToVideo v =
  a ! href   (fromString $ D.url   v)
    $ toHtml (D.title v)

videoDate :: Video -> Html
videoDate v =
  H.div ! class_ "date"
        $ toHtml (showGregorian (D.streamDate v))

videoDescription :: Video -> Html
videoDescription v =
  H.div ! class_ "description"
        $ toHtml (D.description v)

listTags :: [D.Tag] -> Html
listTags []   = mempty
listTags tags =
  H.div ! class_ "tags"
        $ mconcat (displayTags tags)

displayTags :: [D.Tag] -> [Html]
displayTags tags = fmap displayTag tags

displayTag :: D.Tag -> Html
displayTag tag = do
  H.span
    ! class_ "tag"
    $ toHtml (getTag tag)
