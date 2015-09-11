-- | Module for printing out lecture 1 information.
module Main where

import Bahug101Lec1


-- Turn a list of Integers into groups of every two elements.
pairList :: [Integer] -> [(Integer, Integer)]
pairList [] = []
pairList [x] = []
pairList (x : (y : ys)) = (x, y) : pairList ys


main :: IO ()
main = do
  -- Int numbers
  putStrLn $ "i = " ++ show i

  -- Arbitrarily large Integer numbers (limited by machine memory)
  putStrLn $ "n = " ++ show n
  putStrLn $ "reallyBig = " ++ show reallyBig
  putStrLn $ "numDigits = " ++ show numDigits

  -- Floating point, single precision
  putStrLn $ "f1 = " ++ show f1
  putStrLn $ "f2 = " ++ show f2

  -- Floating point, double precision
  putStrLn $ "d1 = " ++ show d1
  putStrLn $ "d2 = " ++ show d2

  -- Booleans
  putStrLn $ "b1 = " ++ show b1
  putStrLn $ "b2 = " ++ show b2

  -- Example characters, including unicode
  putStrLn $ "c1 = " ++ [c1]
  putStrLn $ "c2 = " ++ [c2]
  putStrLn $ "c3 = " ++ [c3]

  -- Strings
  putStrLn $ "s1 = " ++ show s1
  putStrLn $ "s2 = " ++ show s2

  -- Boolean logic examples
  putStrLn $ "ex11 = " ++ show ex11
  putStrLn $ "ex12 = " ++ show ex12
  putStrLn $ "ex12b = " ++ show ex12b
  putStrLn $ "ex13 = " ++ show ex13
  -- print ex13b -- Contains a type error (and is not defined in Bahug101Lec1)
  putStrLn $ "ex14 = " ++ show ex14
  putStrLn $ "ex15 = " ++ show ex15
  putStrLn $ "ex16 = " ++ show ex16

  -- If expressions
  putStrLn $ "ex_a = " ++ show ex_a

  -- Basic sample sum function
  putStrLn $ "sumtorial 5 = " ++ show (sumtorial 5)

  -- Collatz algorithm
  putStrLn $ "hailstone 12" ++ show (hailstone 12)
  putStrLn $ "hailstone' 12" ++ show (hailstone' 12)
  putStrLn $ "hailstone'' 12" ++ show (hailstone'' 12)

  putStrLn $ "hailstone 13" ++ show (hailstone 13)
  putStrLn $ "hailstone' 13" ++ show (hailstone' 13)
  putStrLn $ "hailstone'' 13" ++ show (hailstone'' 13)

  -- Pairs
  putStrLn $ "p = " ++ show p
  putStrLn $ "p' = " ++ show p'

  -- Functions with multiple input arguments
  putStrLn $ "f 1 2 3 = " ++ show (f 1 2 3)
  putStrLn $ "f3 2 3 = " ++ show (f3 2 3)

  -- Lists and Ranges
  putStrLn $ "nums = " ++ show nums
  putStrLn $ "range = " ++ show range
  putStrLn $ "range2 = " ++ show range2
  putStrLn $ "range3 = " ++ show range3

  -- Creating lists
  putStrLn $ "ex18 = " ++ show ex18
  putStrLn $ "ex19 = " ++ show ex19
  putStrLn $ "ex20 = " ++ show ex20
  putStrLn $ "ex21 = " ++ show ex21

  -- List append
  putStrLn $ "exAppend1 = " ++ show exAppend1
  putStrLn $ "exAppend2 = " ++ show exAppend2
  putStrLn $ "exAppend3 = " ++ show exAppend3

  -- Hailstone Sequence
  putStrLn $ "hailstoneSeq 12 = " ++ show (hailstoneSeq 12)
  putStrLn $ "hailstoneSeq 13 = " ++ show (hailstoneSeq 13)

  -- intListLength
  putStrLn $ "intListLength range2 = " ++ show (intListLength range3)
  putStrLn $ "intListLength' range2 = " ++ show (intListLength' range3)

  -- Summing lists!
  putStrLn $ "sumEveryTwo range2 = " ++ show (sumEveryTwo range3)
  putStrLn $ "sumPairs listPairs = " ++ show (sumPairs . pairList $ range2)

  -- Combining functions
  putStrLn $ "hailstoneLen 12 = " ++ show (hailstoneLen 12)
  putStrLn $ "hailstoneLen 13 = " ++ show (hailstoneLen 13)
