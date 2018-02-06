{-# LANGUAGE OverloadedStrings #-}

module Templating
  ( page
  ) where

import           Database                    as D (Video (..))

import           Data.Text                   (Text)
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as HA

page :: [Video] -> Html
page videos =
  html $
    body $
      listVideos videos

listVideos :: [Video] -> Html
listVideos []     = p $ toHtml noVideos
listVideos videos = ul $ mconcat (fmap listItem videos)

noVideos :: Text
noVideos = "No video found :-("

listItem :: Video -> Html
listItem v =
  li $
    H.span
      ! class_ "title"
      $ toHtml (D.title v)
