{-# LANGUAGE OverloadedStrings #-}
module Text.Regex.Fuzzy.Gen
       ( Regex, mkRegex, tryMatch
       ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.Fuzzy.AST
import Text.Regex.Fuzzy.Simpl
import Text.Regex.Fuzzy.Dist

inItems :: [Item] -> Char -> Bool
inItems xs c = any (inItem c) xs

type Matcher = Parser (DistFront Char)


limitDist :: Dist -> DistFront Char -> Matcher
limitDist maxDist df = do
  when (editDist df > maxDist) $ do
    fail ""
  return df

consume :: Char -> DistFront Char -> Matcher
consume expected df = insertBoth expected df <$> anyChar

mkChar :: Char -> Dist -> DistFront Char -> Matcher
mkChar expected maxDist df = consume expected df <|> noInput
  where
    noInput = return (insertLeft expected df)

mkAny :: Dist -> DistFront Char -> Matcher
mkAny maxDist df = consume <|> noInput
  where
    consume = do
      actual <- anyChar
      return (insertBoth actual df actual)

    noInput = return (insertLeft anyChr df)

    anyChr = '\0'

mkSExp :: SExp
       -> Dist           -- ^ maximum edit distance for a string matched by SExp.
       -> DistFront Char -- ^ distance front so far
       -> Matcher
mkSExp (SChar c) md df = mkChar c md df
--mkSExp (SText t) md _ = error "mkSExp" --void (string t) >>
mkSExp (SCat xs) md dfinit = go xs dfinit
  where
    go []       df = return df
    go (x : xs) df = do
      df' <- mkSExp x md df
      go xs df'

mkSExp (SAlt xs)   md df = choice (map (\se -> mkSExp se md df) xs) -- minimize cost?
mkSExp (SCost d s)  _ df = mkSExp s d df >>= limitDist d

mkSExp  SAny       md df = mkAny md df
{-

mkSExp (SPos xs)   md = error "mkSExp" -- void (satisfy (inItems xs))
mkSExp (SNeg _)    md = error "mkSExp"
-}
mkSExp  SEOS       md df = endOfLine >> return df
--mkSExp  SSOS       md = error "mkSExp"
mkSExp  SEmpty     md df = return df

newtype Regex = Regex Matcher

mkRegex :: Exp -> Regex
mkRegex = Regex . (\se -> mkSExp se 0 emptyF) . simpl

tryMatch :: Regex -> Text -> Either String Int
tryMatch (Regex p) t =
  case feed (parse p t) "" of
    Fail _ _ s -> Left s
    Partial _  -> error "tryMatch: impossible happen"
    Done r _   -> Right (T.length t - T.length r)


{-
mkItem :: Item -> Parser ()
mkItem (CharI c)   = void (char c)
mkItem (Range a b) = void (satisfy (\x -> a <= x && x <= b))

mkClass :: Class -> Parser ()
mkClass  AnyC        = void (anyChar)
mkClass (PosSetC xx) = void (satisfy (inItems xx))
mkClass (NegSetC _)  = error "mkClass"

mkAtom :: Atom -> Matcher
mkAtom  SOS       = error "mkAtom"
mkAtom  EOS       = endOfLine >> return 0
mkAtom (CharA c)  = char c    >> return 1
mkAtom (ClassA c) = mkClass c >> return 1
{-# INLINE mkAtom #-}

mkExp :: Exp -> Matcher
mkExp  EmptyE     = return 0
mkExp (AtomE a)   = mkAtom a
mkExp (CatE xx)   = go xx
  where
    go [] = return 0
    go (x : xs) = do
      i  <- mkExp x
      is <- go xs
      return (i + is)

mkExp (AlterE xs) = choice (map mkExp xs)
{-# INLINE mkExp #-}
-}
