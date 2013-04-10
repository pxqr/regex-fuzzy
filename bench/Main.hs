{-#  LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text.IO as T
import Criterion.Main

import Text.Regex.Fuzzy
import Text.Regex.PCRE.ByteString
import System.IO.Unsafe


main :: IO ()
main = do
  let filepath = "bench/lorem-ipsum.txt"
  tt <- T.readFile filepath
  tb <- B.readFile filepath

  Right pcre <- compile 0 0 "_never match"

  defaultMain
       [ mkBench "_never match" tt

       , let f bs = case unsafePerformIO (execute pcre bs) of
               Right r -> r
         in
         bench   "pcre" $ nf f tb
       ]

  where
    mkBench :: String -> Text -> Benchmark
    mkBench r t =
      let reg = regex r in
      bench r $ nf (matches reg) t
