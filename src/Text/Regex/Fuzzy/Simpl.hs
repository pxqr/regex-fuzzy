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

simplE :: Exp -> Exp
simplE _r = fromMaybe _r (go _r)
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


data SExp = SChar Char
          | SText Text
          | SCat  [SExp] -- TODO: array
          | SAlt  [SExp]
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

simplS :: Exp -> SExp
simplS  EmptyE     = SEmpty
simplS (AtomE a)   = simplAtom a
simplS (CatE xs)   = SCat (fmap simplS xs)
simplS (AltE xs)   = SAlt (fmap simplS xs)
simplS (CostE c e) = SCost c (simplS e)


simpl :: Exp -> SExp
simpl = simplS . simplE