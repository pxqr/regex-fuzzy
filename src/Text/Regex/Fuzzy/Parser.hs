-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--
--   This module implement parsing of regular expressions. The following
--   BNF grammar is pretty self-describing:
--
-- > <RE>    ::= <ALT>
-- >
-- > <ALT>   ::= <ALT> "|" <CAT> | <CAT>
-- > <CAT>   ::= <CAT> <ATOM> | <ATOM>
-- > <REP>   ::= <ELEM> <QUAN> | <ELEM>
-- >
-- > <QUAN>  ::= "*" | "+" | "?" | "{" <NUM> "," <NUM> "}"
-- >
-- > <ELEM>  ::= <ATOM> | <GROUP> | <COST>
-- > <GROUP> ::= "(" <RE> ")"
-- > <COST>  ::= "<" <NUM> "|" <RE> ">" "
-- >
-- > <ATOM>  ::= <CHAR> | <CLASS> | <EOL>
-- >
-- > <EOL>   ::= "$"
-- >
-- > <CLASS> ::= <ANY> | <POS> | <NEG>
-- >
-- > <ANY>   ::= "."
-- > <POS>   ::= "["  <ITEMS> "]"
-- > <NEG>   ::= "[^" <ITEMS> "]"
-- >
-- > <ITEMS> ::= <ITEMS> <ITEM> | <ITEM>
-- > <ITEM>  ::= <RANGE> | <CHAR>
-- > <RANGE> ::= <CHAR> "-" <CHAR>
-- >
-- > <CHAR>  ::= not <META> | "\" <META>
-- > <META>  ::= "*" | "+" | "?"
-- >           | "." | "$" | "^" | "-" |"," | "|"
-- >           | "(" | ")" | "<" | ">" | "{" | "}" | "[" | "]"
-- >           | <RESERVED>
-- >
-- > <RESERVED> = "#" | "@" | "_"
-- >
-- > <NUM>   ::= <DIGIT> <NUM> | <NUM>
-- > <DIGIT> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
--
--  However take a note for the COST production rule. We have to enclose
--  arbitrary regex in angle brackets and add /max edit distance/ annotation.
--  So, the regex:
--
--  > <2|Hello>
--
--  means match regex:
--
--  > Hello
--
--  with edit distance not greater than two, otherwise don't match.
--
module Text.Regex.Fuzzy.Parser
       ( -- * Parsing
         parseRE
       ) where

import Control.Applicative ((<$>), (*>), (<*), (<$), (<*>), some, pure)
import Text.Parsec

import Text.Regex.Fuzzy.AST


parseRE :: String -> Either ParseError Exp
parseRE = parse alts "regexp"
  where
    re     = alts
    alts   = altE <$> (seqs `sepBy1` char '|')
    seqs   = catE <$> (some repP) <|> emptyP
    repP   = do
      e <- elemP
      repE <$> pure e <*> quanP <|> return e
      where
        quanP = kleeneP <|> plusP <|> maybeP <|> rangeP
          where
            kleeneP = kleeneQ <$   char '*'
            plusP   = plusQ   <$   char '+'
            maybeP  = maybeQ  <$   char '?'
            rangeP  = inBraces (try bothBoundedP <|>
                                try minBoundedP  <|>
                                try maxBoundedP  <|>
                                try exactlyP
                               )
              where
                exactlyP     = exactlyQ    <$> numP
                minBoundedP  = minBoundedQ <$> (numP <* char ',')
                maxBoundedP  = maxBoundedQ <$> (char ',' *> numP)
                bothBoundedP = rangeQ      <$> numP <*> (char ',' *> numP)
                inBraces = between (char '{') (char '}')


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

    numP = read <$> some digit
    charP = satisfy isChar
    isChar = not . (`elem` metacharacter)
    metacharacter = "|*+?.$[^-]#()<>{}"
