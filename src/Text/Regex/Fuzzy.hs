-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
module Text.Regex.Fuzzy
       ( Regex

         -- * Construction
       , regex

         -- * Matching
       , isMatch, match, matches
       , (=~)
       ) where

import Data.String
import           Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.Fuzzy.Gen
import Text.Regex.Fuzzy.Parser

regex :: String -> Regex
regex str =
  case parseRE str of
    Left  e -> error (show e)
    Right e -> mkRegex e

instance IsString Regex where
  fromString = regex

isMatch :: Regex -> Text -> Bool
isMatch r t = case tryMatch r t of
  Left _ -> False
  Right _ -> True

match :: Regex -> Text -> Maybe Text
match r t = case tryMatch r t of
  Left _ -> Nothing
  Right i -> return (T.take i t)

matches :: Regex -> Text -> [Int]
matches r = go
  where
    go t = case tryMatch r t of
      Right str -> str : if T.null t then [] else go (T.tail t)
      Left _  -> if T.null t then [] else go (T.tail t)

(=~) :: Text -> Regex -> Bool
(=~) = flip isMatch
