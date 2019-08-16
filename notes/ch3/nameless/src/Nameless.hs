module Nameless where

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
  , show (I.run example11) == "3628800"
  , show (I.run example12) == "1"
  , show (I.run example13) == "55"
  , show (I.run example14) == "55"
  , show (I.run example15) == "55"
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
  \ let makemult = proc (maker)                                       \
  \                  proc (x)                                         \
  \                    proc (y)                                       \
  \                      if zero?(x)                                  \
  \                      then 0                                       \
  \                      else -((((maker maker) -(x, 1)) y), -(0, y)) \
  \ in let mult = proc (x) proc (y) (((makemult makemult) x) y)       \
  \    in let makefact = proc (maker)                                 \
  \                        proc (n)                                   \
  \                          if zero?(n)                              \
  \                          then 1                                   \
  \                          else ((mult n) ((maker maker) -(n, 1)))  \
  \       in let fact = proc (n) ((makefact makefact) n)              \
  \          in (fact 10)                                             "

example12 :: String
example12 = "\
  \ let makeodd = proc (makerodd)                                          \
  \                 proc (makereven)                                       \
  \                   proc (n)                                             \
  \                     if zero?(n)                                        \
  \                     then 0                                             \
  \                     else (((makereven makereven) makerodd) -(n, 1))    \
  \ in let makeeven = proc (makereven)                                     \
  \                     proc (makerodd)                                    \
  \                       proc (n)                                         \
  \                         if zero?(n)                                    \
  \                         then 1                                         \
  \                         else (((makerodd makerodd) makereven) -(n, 1)) \
  \    in let odd = proc (n) (((makeodd makeodd) makeeven) n)              \
  \       in let even = proc (n) (((makeeven makeeven) makeodd) n)         \
  \          in (odd 13)                                                   "

example13 :: String
example13 = "\
  \ let makesum = proc (maker)                                 \
  \                 proc (n)                                   \
  \                   if zero?(n)                              \
  \                   then 0                                   \
  \                   else -(n, -(0, ((maker maker) -(n, 1)))) \
  \ in let sum = (makesum makesum)                             \
  \    in (sum 10)                                             "

example14 :: String
example14 = "\
  \ let makesum = proc (sum)                         \
  \                 proc (n)                         \
  \                   if zero?(n)                    \
  \                   then 0                         \
  \                   else -(n, -(0, (sum -(n, 1)))) \
  \ in let makesumrec = proc (maker)                 \
  \                       (makesum (maker maker))    \
  \    in let sum = (makesumrec makesumrec)          \
  \       in (sum 10)                                "

example15 :: String
example15 = "\
  \ let makerec = proc (makef)                             \
  \                 let makefrec = proc (maker)            \
  \                                  (makef (maker maker)) \
  \                 in (makefrec makefrec)                 \
  \ in let makesum = proc (sum)                            \
  \                    proc (n)                            \
  \                      if zero?(n)                       \
  \                      then 0                            \
  \                      else -(n, -(0, (sum -(n, 1))))    \
  \    in let sum = (makerec makesum)                      \
  \       in (sum 10)                                      "
