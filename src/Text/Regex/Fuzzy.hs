module Text.Regex.Fuzzy
       ( Regex

         -- ^ Construction
       , regexp

         -- ^ Matching
       , isMatch, match, matches
       , (=~)
       ) where

import Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.Fuzzy.AST
import Text.Regex.Fuzzy.Gen
import Text.Regex.Fuzzy.Parser

regexp :: String -> Regex
regexp str =
  case parseRE str of
    Left  e -> error (show e)
    Right e -> mkRegex e


isMatch :: Regex -> Text -> Bool
isMatch = undefined


match = tryMatch

matches :: Regex -> Text -> [Text]
matches = undefined

(=~) :: Text -> Regex -> Bool
(=~) = flip isMatch
