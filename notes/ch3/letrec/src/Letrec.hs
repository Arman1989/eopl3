module Letrec where

import Interpreter as I

result :: Bool
result = and tests

tests :: [Bool]
tests =
  [ show (I.run example1) == "5"
  , show (I.run example2) == "10"
  , show (I.run example3) == "56"
  , show (I.run example4) == "#f"
  , show (I.run example5) == "#t"
  , show (I.run example6) == "2"
  , show (I.run example7) == "3"
  , show (I.run example8) == "-5"
  , show (I.run example9) == "55"
  , show (I.run example10) == "55"
  , show (I.run example11) == "12"
  , show (I.run example12) == "5050"
  ]

-- Examples

example1 :: String
example1 = "5"

example2 :: String
example2 = "x"

example3 :: String
example3 = "-(55, -(x, 11))"

example4 :: String
example4 = "zero?(i)"

example5 :: String
example5 = "zero?(-(i, 1))"

example6 :: String
example6 = "let x = 5 in -(x, 3)"

example7 :: String
example7 = "                        \
  \ let z = 5 in                    \
  \   let x = 3 in                  \
  \     let y = -(x, 1) in          \
  \       let x = 4 in -(z, -(x, y))"

example8 :: String
example8 = "                                  \
  \ let x = 7 in                              \
  \   let y = 2 in                            \
  \     let y = let x = -(x, 1) in -(x, y) in \
  \       -(-(x, 8), y)                       "

example9 :: String
example9 = "let f = proc (x) -(x, 11) in (f (f 77))"

example10 :: String
example10 = "(proc (f) (f (f 77)) proc (x) -(x, 11))"

example11 :: String
example11 = "\
  \ letrec double(x) = if zero?(x) then 0 else -((double -(x, 1)), -2) \
  \ in (double 6)                                                      "

example12 :: String
example12 = "\
  \ letrec sum (n) = if zero?(n) then 0 else -(n, -(0, (sum -(n, 1)))) \
  \ in (sum 100)                                                       "
