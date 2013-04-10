module Text.Regex.Fuzzy.Simpl
       ( SExp(..)
       , simpl
       ) where

import Control.Applicative
import Data.Array
--import Control.Monad.State
import Data.Maybe
import Data.Text (Text)
import Text.Regex.Fuzzy.AST

--type Fuel = State Int

--withFuel :: Fuel a -> Int -> a
--withFuel = evalState

-- CPS?
data SExp = SChar Char
          | SText Text
          | SCat  (Array Int SExp) -- TODO: array
          | SAlt  (Array Int SExp)
          | SCost Int SExp
          | SAny
          | SPos [Item]
          | SNeg [Item]
          | SEOS
          | SSOS
          | SEmpty
            deriving Show

simplClass :: Class -> SExp
simplClass  AnyC       = SAny
simplClass (PosSetC s) = SPos s
simplClass (NegSetC s) = SNeg s

simplAtom :: Atom -> SExp
simplAtom  EOS       = SEOS
simplAtom  SOS       = SSOS
simplAtom (CharA  c) = SChar c
simplAtom (ClassA c) = simplClass c

simplMany :: [Exp] -> Array Int SExp
simplMany xs = fmap simpl (listArray (0, length xs - 1) xs)

simpl :: Exp -> SExp
simpl  EmptyE     = SEmpty
simpl (AtomE a)   = simplAtom a
simpl (CatE xs)   = SCat (simplMany xs)
simpl (AlterE xs) = SAlt (simplMany xs)
simpl (CostE c e) = SCost c (simpl e)
{-
simplify :: Exp -> Exp
simplify _r = fromMaybe _r (go _r)
  where
    refine r = go r <|> Just r

    goRec2 :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Maybe Exp
    goRec2 con r1 r2 =
      case (go r1, go r2) of
        (Nothing,  Nothing ) -> Nothing
        (Just r1', Just r2') -> refine (con r1' r2')
        (Just r1', Nothing ) -> refine (con r1' r2 )
        (Nothing,  Just r2') -> refine (con r1  r2')

    go :: Exp -> Maybe Exp
    go (CatE xs) = Nothing
    go r = return r
    -- prefix tree

    zipPrefix :: Exp -> Exp -> (Exp, (Exp, Exp))
    zipPrefix = undefined
-}