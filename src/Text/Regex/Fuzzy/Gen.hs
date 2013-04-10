module Text.Regex.Fuzzy.Gen
       ( Regex, mkRegex, tryMatch
       ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.Fuzzy.AST

mkItem :: Item -> Parser ()
mkItem (CharI c)   = void (char c)
mkItem (Range a b) = void (satisfy (\x -> a <= x && x <= b))

mkClass :: Class -> Parser ()
mkClass  AnyC        = void (anyChar)
mkClass (PosSetC xx) = void (satisfy (inItems xx))
  where
    inItems :: [Item] -> Char -> Bool
    inItems xs c = any (inItem c) xs

mkClass (NegSetC _)  = error "mkClass"

type Matcher = Parser Int

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

newtype Regex = Regex Matcher

mkRegex :: Exp -> Regex
mkRegex = Regex . mkExp

tryMatch :: Regex -> Text -> Either String Int
tryMatch (Regex p) t = parseOnly p t
