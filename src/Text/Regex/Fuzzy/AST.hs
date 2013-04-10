module Text.Regex.Fuzzy.AST
       ( Item(CharI, Range)
       , charI, rangeI
       , inItem

       , Class(AnyC, PosSetC, NegSetC)
       , anyC, posSet, negSet

       , Atom(EOS, SOS, CharA, ClassA)
       , eos, sos, charA, classA

       , Exp(EmptyE, AtomE, CatE, AlterE, CostE)
       , emptyE, atomE, catE, alterE, costE

       , Grammar(..)
       ) where

import Data.List
import Data.Monoid
import Data.Ord

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
  NegSetC a == PosSetC b = False -- TODO
  NegSetC a == PosSetC b = False -- TODO
  _         == _         = False

instance Grammar Class where
  AnyC `isSubsetOf` AnyC      = True
  AnyC `isSubsetOf` PosSetC _ = False -- TODO
  AnyC `isSubsetOf` NegSetC _ = False
  _    `isSubsetOf` _         = False

anyC :: Class
anyC = AnyC

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
mergeSet (a : b : xs)
  | Just c <- merge a b = mergeSet (c : xs)
  |       otherwise     = a : mergeSet (b : xs)

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

eos :: Atom
eos = EOS

sos :: Atom
sos = SOS

charA :: Char -> Atom
charA = CharA

classA :: Class -> Atom
classA = ClassA

type Cost = Int

data Exp = EmptyE
         | AtomE  Atom
         | CatE   [Exp]
         | AlterE [Exp]
         | CostE   Cost Exp
--         | KleeneE   Exp
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
catE xs  = CatE xs

alterE :: [Exp] -> Exp
alterE []  = EmptyE
alterE [x] = x
alterE xs  = AlterE xs

costE :: Cost -> Exp -> Exp
costE = CostE