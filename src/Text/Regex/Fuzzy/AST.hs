module Text.Regex.Fuzzy.AST
       ( Item(CharI, Range)
       , charI, rangeI
       , inItem

       , Class(AnyC, PosSetC, NegSetC)
       , anyC, posSet, negSet

       , Atom(EOS, SOS, CharA, ClassA)
       , eos, sos, charA, classA

       , Quan(Quan)
       , kleeneQ, plusQ, maybeQ, rangeQ

       , Exp(EmptyE, AtomE, CatE, AltE, RepE, CostE)
       , emptyE, atomE, catE, altE, costE, repE
       , isEmptyE

       , Grammar(..)
       ) where

import Data.List
import Data.Monoid
import Data.Ord

import Text.Regex.Fuzzy.Dist


class Grammar g where
  isSubsetOf :: g -> g -> Bool

data Item  = CharI Char
           | Range Char Char
             deriving (Show)

instance Eq Item where
  CharI c   == CharI c'  = c == c'
  CharI c   == Range a b = c == a && c == b
  Range a b == CharI c   = c == a && c == b
  Range a b == Range c d = a == c && b == d

instance Grammar Item where
  CharI c   `isSubsetOf` CharI c'  = c == c'
  CharI c   `isSubsetOf` Range a b = a <= c && c <= b
  Range a b `isSubsetOf` CharI c   = a == c && b == c
  Range a b `isSubsetOf` Range c d = c <= a && b <= d

charI :: Char -> Item
charI = CharI

rangeI :: Char -> Char -> Item
rangeI a b
  | a == b    = CharI a
  | otherwise = Range (min a b) (max a b)

inItem :: Char -> Item -> Bool
inItem c (CharI c' ) = c' == c
inItem c (Range a b) = a <= c && c <= b
{-# INLINE inItem #-}

minOf :: Item -> Char
minOf (CharI c) = c
minOf (Range a _)  = a

maxOf :: Item -> Char
maxOf (CharI c) = c
maxOf (Range _ a) = a

isJustCharI :: Item -> Maybe Char
isJustCharI (CharI c)   = Just c
isJustCharI (Range a b) | a == b = Just a
isJustCharI _           = Nothing

data Class = AnyC
           | PosSetC   [Item]
           | NegSetC   [Item]
             deriving Show

instance Eq Class where
  AnyC      == AnyC      = True
  PosSetC a == PosSetC b = a == b
  NegSetC a == NegSetC b = a == b
  NegSetC _ == PosSetC _ = False -- TODO
  _         == _         = False

instance Grammar Class where
  AnyC `isSubsetOf` AnyC      = True
  AnyC `isSubsetOf` PosSetC _ = False -- TODO
  AnyC `isSubsetOf` NegSetC _ = False
  _    `isSubsetOf` _         = False

anyC :: Class
anyC = AnyC

isElemOfClass :: Char -> Class -> Bool
isElemOfClass _  AnyC        = True
isElemOfClass c (PosSetC xs) = (c `inItem`) `any` xs
isElemOfClass _ (NegSetC _)  = error "isElemOfClass"

isJustCharC :: Class -> Maybe Char
isJustCharC (PosSetC [i]) = isJustCharI i
isJustCharC _             = Nothing

merge :: Item -> Item -> Maybe Item
merge a b
  | maxOf a >= minOf b = Just (rangeI (minOf a `min` minOf b)
                                      (maxOf a `max` maxOf b))
  | otherwise = Nothing

mergeSet :: [Item] -> [Item]
mergeSet [] = []
mergeSet [x] = [x]
mergeSet (a : b : xs) =
  case merge a b of
    Nothing -> a : mergeSet (b : xs)
    Just c  -> mergeSet (c : xs)

posSet :: [Item] -> Class
posSet = PosSetC . mergeSet . sortBy (comparing minOf)

negSet :: [Item] -> Class
negSet = error "negSet"


-- One character long.
data Atom = EOS
          | SOS
          | CharA Char
          | ClassA Class
            deriving Show

instance Eq Atom where
  EOS       == EOS       = True
  SOS       == SOS       = True
  CharA c   == CharA c'  = c == c'
  CharA c   == ClassA cl = isJustCharC cl == Just c
  ClassA cl == CharA  c  = isJustCharC cl == Just c
  ClassA a  == ClassA b  = a == b
  _         == _         = False

instance Grammar Atom where
  EOS      `isSubsetOf` EOS       = True
  SOS      `isSubsetOf` SOS       = True
  CharA c  `isSubsetOf` CharA c'  = c == c'
  CharA c  `isSubsetOf` ClassA cl = c `isElemOfClass` cl
  ClassA c `isSubsetOf` CharA  cr = isJustCharC c == Just cr
  ClassA a `isSubsetOf` ClassA b  = a == b
  p        `isSubsetOf` q         = p == q -- failback

eos :: Atom
eos = EOS

sos :: Atom
sos = SOS

charA :: Char -> Atom
charA = CharA

classA :: Class -> Atom
classA = ClassA

data Quan = Quan Int Int
           deriving (Show, Eq)

kleeneQ :: Quan
kleeneQ = Quan 0 maxBound

plusQ :: Quan
plusQ = Quan 1 maxBound

maybeQ :: Quan
maybeQ = Quan 0 1

rangeQ :: Int -> Int -> Quan
rangeQ = Quan

data Exp = EmptyE
         | AtomE  Atom
         | CatE   [Exp]
         | AltE   [Exp]
         | CostE   Dist Exp
         | RepE    Exp  Quan
--         | LetE   Name Exp
--         | AsE       Name Exp
           deriving (Show, Eq)

--instance Semigroup Exp where
--  (<+>) = AlterE

instance Monoid Exp where
    mempty      = emptyE
    mappend a b = catE [a, b]
    mconcat     = catE

emptyE :: Exp
emptyE = EmptyE

atomE :: Atom -> Exp
atomE = AtomE

catE :: [Exp] -> Exp
catE []  = EmptyE
catE [x] = x
catE [x, EmptyE] = x
catE (EmptyE : xs) = catE xs
catE xs  = CatE xs

altE :: [Exp] -> Exp
altE []  = EmptyE
altE [x] = x
altE xs  = AltE xs

costE :: Dist -> Exp -> Exp
costE = CostE

repE :: Exp -> Quan -> Exp
repE = RepE

isEmptyE :: Exp -> Bool
isEmptyE EmptyE = True
isEmptyE _      = False