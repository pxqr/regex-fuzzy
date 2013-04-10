module Text.Regex.Fuzzy.Gen
       ( Regex, mkRegex, tryMatch
       ) where

import Control.Applicative
import Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as T
import Text.Regex.Fuzzy.AST

mkItem :: Item -> Parser Char
mkItem (CharI c)   = char c
mkItem (Range a b) = satisfy (\x -> a <= x && x <= b)

mkClass :: Class -> Parser Char
mkClass  AnyC        = anyChar
mkClass (PosSetC xs) = satisfy (inItems xs)
  where
    inItems :: [Item] -> Char -> Bool
    inItems xs c = any (inItem c) xs

mkClass _            = error "mkClass"

type Matcher = Parser String

mkAtom :: Atom -> Matcher
mkAtom  SOS       = error "mkAtom"
mkAtom  EOS       = endOfLine >> return []
mkAtom (CharA c)  = char c    >>= \x -> return [x]
mkAtom (ClassA c) = mkClass c >>= \x -> return [x]

mkExp :: Exp -> Matcher
mkExp  EmptyE     = return []
mkExp (AtomE a)   = mkAtom a
mkExp (CatE xs)   = concat <$>  mapM mkExp xs
mkExp (AlterE xs) = choice (map mkExp xs)


newtype Regex = Regex { unRegex :: Matcher }

mkRegex :: Exp -> Regex
mkRegex = Regex . mkExp

tryMatch (Regex p) t =
  case parseOnly p t of
    Left _   -> Nothing
    Right xs -> Just xs