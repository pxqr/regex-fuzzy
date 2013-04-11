module Text.Regex.Fuzzy.Parser
       ( parseRE
       ) where

import Control.Applicative ((<$>), (*>), (<*), (<$), (<*>), some)
import Text.Parsec

import Text.Regex.Fuzzy.AST

{-
<RE>   ::= <ALT>
<ALT>  ::= <ALT> "|" <CAT> | <CAT>
<CAT>  ::= <CAT> <ATOM> | <ATOM>
<ATOM> ::=
-}

parseRE :: String -> Either ParseError Exp
parseRE = parse alts "regexp"
  where
    re     = alts
    alts   = altE <$> (seqs `sepBy1` char '|')
    seqs   = catE   <$> (some elemP) <|> emptyP
    emptyP = return emptyE

    elemP  = costP <|> groupP <|> atomP
      where
        costP   = char '<' *> (CostE <$> (annP <* char '|') <*> re) <* char '>'
        groupP  = char '(' *> re <* char ')'
        annP    = read <$> some digit


    atomP  = atomE  <$> (charA <$> charP
                    <|>  eosP
                    <|>  sosP
                    <|>  classA <$> classP)
      where
        sosP   = sos   <$  char '^'
        eosP   = eos   <$  char '$'

        classP = anyP <|> posP <|> negP
          where
            anyP   = anyC   <$  char '.'
            posP   = posSet <$> (char   '['   *> itemsP <* char ']')
            negP   = negSet <$> (string "[^"  *> itemsP <* char ']')

            itemsP = many itemP
            itemP  = rangeP <|> charI <$> charP

            rangeP = rangeI <$> (charP <* char '-') <*> charP

    charP = satisfy isChar
    isChar = not . (`elem` metacharacter)
    metacharacter = "|*+?.$[^-]#()<>"
