module Text.Regex.Fuzzy
       ( Regex

         -- ^ Construction
       , regex

         -- ^ Matching
       , isMatch, match, matches
       , (=~)
       ) where

import Control.Applicative
import Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.Fuzzy.Gen
import Text.Regex.Fuzzy.Parser

regex :: String -> Regex
regex str =
  case parseRE str of
    Left  e -> error (show e)
    Right e -> mkRegex e


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
