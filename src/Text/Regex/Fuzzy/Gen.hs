module Text.Regex.Fuzzy.Gen
       ( Regex, mkRegex, tryMatch
       ) where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.Fuzzy.AST
import Text.Regex.Fuzzy.Simpl

inItems :: [Item] -> Char -> Bool
inItems xs c = any (inItem c) xs

type Matcher = Parser Int

mkSExp :: SExp -> Matcher
mkSExp (SChar c) = char c >> return 1
mkSExp (SText t) = string t >> return (T.length t)
mkSExp (SCat xs) = go (snd (bounds xs))
  where
    go 0 = mkSExp (xs ! 0)
    go n = do
      i  <- mkSExp (xs ! n)
      is <- go (pred n)
      return (i + is)

mkSExp (SAlt xs) = error "mkSExp" -- choice (map mkSExp xs)
mkSExp (SCost c s) = error "mkSExp"
mkSExp  SAny     = anyChar >> return 1
mkSExp (SPos xs) = satisfy (inItems xs) >> return 1
mkSExp (SNeg _)  = error "mkSExp"
mkSExp  SEOS     = endOfLine >> return 0
mkSExp  SSOS     = error "mkSExp"
mkSExp  SEmpty   = return 0

newtype Regex = Regex Matcher

mkRegex :: Exp -> Regex
mkRegex = Regex . mkSExp . simpl

tryMatch :: Regex -> Text -> Either String Int
tryMatch (Regex p) t = parseOnly p t


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
