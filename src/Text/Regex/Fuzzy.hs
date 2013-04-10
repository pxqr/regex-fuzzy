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
isMatch r t = isJust (tryMatch r t)

match :: Regex -> Text -> Maybe Text
match r t = T.pack <$> tryMatch r t

matches :: Regex -> Text -> [Text]
matches r = catMaybes . go
  where
    go :: Text -> [Maybe Text]
    go t = match r t : case T.uncons t of
      Just (_, t') -> go t'
      Nothing      -> []

(=~) :: Text -> Regex -> Bool
(=~) = flip isMatch
