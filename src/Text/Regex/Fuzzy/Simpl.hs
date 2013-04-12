module Text.Regex.Fuzzy.Simpl
       ( SExp(..)
       , simpl, simplS, simplE
       ) where

import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import Text.Regex.Fuzzy.AST

--type Fuel = State Int

--withFuel :: Fuel a -> Int -> a
--withFuel = evalState

flatCats :: Exp -> [Exp]
flatCats (CatE xs) = concatMap flatCats xs
flatCats e         = [e]

simplE :: Exp -> Exp
simplE = go
  where
    go :: Exp -> Exp
    go (CatE [ ]) = emptyE
    go (CatE [x]) = go x
    go (CatE xxs) = catE (flatCats (catE (filter (not . isEmptyE) (map go xxs))))

    go (AltE [ ]) = emptyE
    go (AltE [x]) = go x
    go (AltE xxs) = catE (flatCats (zipPrefs (map go (flatAlts xxs))))
      where
        flatAlts :: [Exp] -> [Exp]
        flatAlts = concatMap ext
          where
            ext (AltE xs) = xs
            ext e         = [e]

        zipPref :: Exp -> Exp -> (Exp, (Exp, Exp))
        zipPref (CatE (AtomE x : xs)) (CatE (AtomE y : ys))
          | y `isSubsetOf` x =
            let (p, suffs) = zipPref (catE xs) (catE ys)
            in (catE [atomE x, p], suffs)
        zipPref a b = (emptyE, (a, b))

        zipPrefs :: [Exp] -> Exp
        zipPrefs []  = emptyE
        zipPrefs [x] = x
        zipPrefs (a : b : xs) =
          let (p, (s1, s2)) = zipPref a b in
          zipPrefs (catE [p, altE [s1, s2]] : xs)

    go (RepE  e q) = RepE (go e) q
    go (CostE c e) = CostE c (go e)
    go r = r


data SExp = SChar Char
          | SCat  [SExp] -- TODO: array
          | SAlt  [SExp]
          | SRep   SExp Quan
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
simplS (RepE e q)  = SRep (simplS e) q
simplS (CostE c e) = SCost c (simplS e)


simpl :: Exp -> SExp
simpl = simplS . simplE