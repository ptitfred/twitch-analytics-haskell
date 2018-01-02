{-# LANGUAGE TemplateHaskell #-}

{- This is a fork of https://hackage.haskell.org/package/string-qq-0.0.2/docs/src/Data-String-QQ.html#s
 - to let us encode in UTF-8.
 -}

module UTF8QQ
  ( utf8
  ) where

import           Data.ByteString.Lazy      (ByteString, fromStrict)
import           Data.Text                 (pack)
import           Data.Text.Encoding        (encodeUtf8)
import           Language.Haskell.TH.Quote

-- | Helper to build multiline strings with UTF8 support.
--
-- Example:
--
-- @
-- myJSON =
--    [utf8|
--       {
--         text: "This is a UTF-8 ready text, Frédéric"
--       }
--    |]
-- @
utf8 :: QuasiQuoter
utf8 = QuasiQuoter ((\a -> [|asUTF8 a|]) . trimLeadingNewline . removeCRs)
                 (error "Cannot use q as a pattern")
                 (error "Cannot use q as a type")
                 (error "Cannot use q as a dec")
  where
    removeCRs = filter (/= '\r')
    trimLeadingNewline ('\n':xs) = xs
    trimLeadingNewline xs        = xs

asUTF8 :: String -> ByteString
asUTF8 = fromStrict . encodeUtf8 . pack
