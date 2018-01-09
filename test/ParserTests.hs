{-# LANGUAGE QuasiQuotes #-}

module ParserTests
  ( test
  ) where

import           TwitchParser                   (interpretMetadatas)

import           Data.Aeson                     (eitherDecode)
import           Data.ByteString.Lazy           (ByteString)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (Assertion, (@?=))
import           UTF8QQ                         (utf8)

test :: Test
test = testGroup "Parser"
  [ testCase "With a valid json response"  testValidExample
  , testCase "With an empty json response" testEmptyExample
  , testCase "With an invalid response"    testErrorExample
  ]

extractViewerCount :: ByteString -> Maybe Int
extractViewerCount = interpretMetadatas . eitherDecode

testValidExample :: Assertion
testValidExample = extractViewerCount example @?= Just 15

testEmptyExample :: Assertion
testEmptyExample = extractViewerCount emptyDataExample @?= Nothing

testErrorExample :: Assertion
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
