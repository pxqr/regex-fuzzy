module Main (main) where

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Text.Regex.Fuzzy.Dist


prop_LenDiff :: String -> String -> Bool
prop_LenDiff a b = findEditDist a b >= length a - length b

prop_LenMax :: String -> String -> Bool
prop_LenMax a b = findEditDist a b <= (length a `max` length b)

prop_Eq :: String -> String -> Bool
prop_Eq a b = (findEditDist a b == 0) == (a == b)

prop_Triangle :: String -> String -> String -> Bool
prop_Triangle a b c = findEditDist a b <= (findEditDist a c + findEditDist b c)


main :: IO ()
main = defaultMain
       [ testProperty "more that len diff"         prop_LenDiff
       , testProperty "at most length of longest"  prop_LenMax
       , testProperty "is zero then strs is eq"    prop_Eq
       , testProperty "triangle"                   prop_Triangle
       ]