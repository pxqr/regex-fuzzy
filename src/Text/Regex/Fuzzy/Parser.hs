module Text.Regex.Fuzzy.Parser
       ( parseRE
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
            kleeneP = kleeneQ <$ char '*'
            plusP   = plusQ   <$ char '+'
            maybeP  = maybeQ  <$ char '?'
            rangeP  = rangeQ  <$> (char '{' *> numP)
                              <*> (char ',' *> numP <* char '}')


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
    metacharacter = "|*+?.$[^-]#()<>"
