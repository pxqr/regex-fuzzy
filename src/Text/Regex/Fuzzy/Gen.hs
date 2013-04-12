{-# LANGUAGE OverloadedStrings #-}
module Text.Regex.Fuzzy.Gen
       ( Regex, mkRegex, tryMatch
       ) where

import Control.Applicative
import Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.Fuzzy.AST
import Text.Regex.Fuzzy.Simpl
import Text.Regex.Fuzzy.Dist


type Matcher = Parser (DistFront Char)


limitDist :: Dist -> DistFront Char -> Matcher
limitDist maxDist df
  | editDist df > maxDist = fail ""
  | otherwise             = return df

assertDist :: Dist -> DistFront Char -> Matcher
assertDist 0 df
  | editDist df > 0 = fail ""
assertDist _ df = return df

consume :: Char -> DistFront Char -> Matcher
consume expected df = insertBoth expected df <$> anyChar

mkChar :: Char -> DistFront Char -> Matcher
mkChar expected df = consume expected df <|> noInput
  where
    noInput = return (insertLeft expected df)

mkAny :: DistFront Char -> Matcher
mkAny df = consumeA <|> noInput
  where
    consumeA = do
      actual <- anyChar
      return (insertBoth actual df actual)

    noInput = return (insertLeft anyChr df)

    anyChr = '\0'

mkRep :: Int -> Int -> DistFront Char -> (DistFront Char -> Matcher) -> Matcher
mkRep minCount maxCount dfi p = req minCount dfi >>= opt (maxCount - minCount)
  where
    req 0 df = return df
    req n df = p df >>= req (pred n)

    opt 0 df = return df
    opt n df = (p df >>= opt (pred n))
           <|> return df

mkSExp :: SExp
       -> Dist           -- ^ maximum edit distance for a string matched by SExp.
       -> DistFront Char -- ^ distance front so far
       -> Matcher
mkSExp (SChar c) md df = mkChar c df >>= assertDist md
mkSExp (SCat xs) md dfinit = go xs dfinit
  where
    go []       df = return df
    go (x : xxs) df = do
      df' <- mkSExp x md df
      go xxs df'

mkSExp (SAlt xs)   md df = choice (map (\se -> mkSExp se md df) xs) -- minimize cost?
mkSExp (SRep e (Quan mi ma)) md df = mkRep mi ma df (mkSExp e md)
mkSExp (SCost d s)  _ df = mkSExp s d df >>= limitDist d

mkSExp  SAny       _  df = mkAny df
{-
mkSExp (SPos xs)   md = error "mkSExp" -- void (satisfy (inItems xs))
mkSExp (SNeg _)    md = error "mkSExp"
-}
mkSExp  SEOS       _  df = endOfInput >> return df
--mkSExp  SSOS       md = error "mkSExp"
mkSExp  SEmpty     _  df = return df

newtype Regex = Regex Matcher

mkRegex :: Exp -> Regex
mkRegex = Regex . (\se -> mkSExp se 0 emptyF) . simpl

tryMatch :: Regex -> Text -> Either String Int
tryMatch (Regex p) t =
  case feed (parse p t) "" of
    Fail _ _ s -> Left s
    Partial _  -> error "tryMatch: impossible happen"
    Done r _   -> Right (T.length t - T.length r)
