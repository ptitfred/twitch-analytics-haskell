module Service
  ( main
  ) where

import           Database                 (newPool)
import           Server                   (application)

import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  pool <- newPool
  run 8080 (application pool)
