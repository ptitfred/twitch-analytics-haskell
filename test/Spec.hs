module Spec
  ( main
  ) where

import           ParserTests

import           Test.Framework

main :: IO ()
main = defaultMain [Spec.test]

test :: Test
test = testGroup "twitch-analytics-haskell"
  [ ParserTests.test
  ]
