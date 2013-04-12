-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# OPTIONS -fno-warn-orphans #-}
module Text.Regex.Fuzzy
       ( Regex

         -- * Construction
       , regex

         -- * Matching
       , isMatch, matchOnce, matchAll
       , matchLen, matchPos, matchCount
       , (=~), (=~?)
       ) where

import Control.Applicative
import Data.Maybe
import Data.String
import           Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.Fuzzy.Gen
import Text.Regex.Fuzzy.Parser

-- | Build regex from a string using default options.
regex :: String -> Regex
regex str =
  case parseRE str of
    Left  e -> error (show e)
    Right e -> mkRegex e

instance IsString Regex where
  fromString = regex


-- | T
isMatch :: Regex -> Text -> Bool
isMatch r t = isJust (tryMatch r t)

matchOnce :: Regex -> Text -> Maybe Text
matchOnce r t = (`T.take` t) <$> tryMatch r t

matchAll :: Regex -> Text -> [Text]
matchAll r = go
  where
    go t = case matchOnce r t of -- catMaybes is significant slower
      Just str -> str : rest
      Nothing  -> rest
     where
        rest = if T.null t then [] else go (T.tail t)

-- | Zero based index from start of a source string.
type MatchOfst = Int

-- | Length of match.
type MatchLen  = Int

-- | Position in a source string of a matched string.
type Match = (MatchOfst, MatchLen)

matchPos :: Regex   -- ^ Regex to match
           -> Text    -- ^ Source string.
           -> [Match] -- ^ All possible matches.
matchPos r = go 0
  where
    go n t = case tryMatch r t of
      Just l  -> (l, n) : rest
      Nothing -> rest
     where
       rest = if T.null t then [] else go (succ n) (T.tail t)

matchLen :: Regex -> Text -> Maybe Int
matchLen = tryMatch

matchCount :: Regex -> Text -> Int
matchCount r = length . matchPos r

-- | Operator version of 'match'.
(=~) :: Text -> Regex -> Maybe Text
(=~) = flip matchOnce

infix 5 =~

-- | Operator version of 'isMatch'.
(=~?) :: Text -> Regex -> Bool
(=~?) = flip isMatch

infix 4 =~?
