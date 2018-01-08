{-# LANGUAGE QuasiQuotes #-}

module ParserTests
  ( test
  ) where

import           TwitchParser                   (extractViewerCount)

import           Data.ByteString.Lazy           (ByteString)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     ((@?=))
import           UTF8QQ                         (utf8)

test :: Test
test = testGroup "Parser"
  [ testCase "With a valid json response" testValidExample,
    testCase "With an empty json response" testEmptyExample,
    testCase "With an invalid response" testErrorExample
  ]

testValidExample = extractViewerCount example @?= Just 15
testEmptyExample = extractViewerCount emptyDataExample @?= Nothing
testErrorExample = extractViewerCount errorExample @?= Nothing

emptyDataExample :: ByteString
emptyDataExample =
  [utf8|
    {
      "data": []
    }
  |]

errorExample :: ByteString
errorExample =
  [utf8|
    Internal Server Error
  |]

example :: ByteString
example =
  [utf8|
    {
      "data": [
        {
          "id": "27150262624",
          "user_id": "30854278",
          "game_id": "458688",
          "community_ids": [],
          "type": "live",
          "title": "Livecoding Haskell pour débutants [FR]",
          "viewer_count": 15,
          "started_at": "2018-01-02T20:15:08Z",
          "language": "fr",
          "thumbnail_url": "https://static-cdn.jtvnw.net/previews-ttv/live_user_ptit_fred-{width}x{height}.jpg"
        }
      ],
      "pagination": {
        "cursor": "eyJiIjpudWxsLCJhIjp7Ik9mZnNldCI6MX19"
      }
    }
  |]
