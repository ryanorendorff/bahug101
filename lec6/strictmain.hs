module Main where

import qualified Bahug101Lec6 as L

main :: IO ()
main = print (L.strictSum [1..1000000])
