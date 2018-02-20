{-# LANGUAGE OverloadedStrings #-}

module Templating
  ( page
  , TaggedVideo
  ) where

import           Database                    as D (Tag (..), TaggedVideo, Video (..))

import           Data.List                   (intersperse)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as HA

page :: [TaggedVideo] -> Html
page videos =
  html $
    body $
      listVideos videos

listVideos :: [TaggedVideo] -> Html
listVideos []     = p $ toHtml noVideos
listVideos videos = ul $ mconcat (fmap listItem videos)

noVideos :: Text
noVideos = "No video found :-("

listItem :: TaggedVideo -> Html
listItem (v, tags) =
  li $ do
    H.span
      ! class_ "title"
      $ toHtml (D.title v)
    listTags tags

listTags :: [D.Tag] -> Html
listTags []   = mempty
listTags tags = " - " <> mconcat (displayTags tags)

displayTags :: [D.Tag] -> [Html]
displayTags tags = intercalateSeparator (fmap displayTag tags)

intercalateSeparator :: [Html] -> [Html]
intercalateSeparator = intersperse tagSeparator

tagSeparator :: Html
tagSeparator = ", "

displayTag :: D.Tag -> Html
displayTag tag = do
  H.span
    ! class_ "tag"
    $ toHtml (getTag tag)
