module Let where

import Interpreter as I

result :: Bool
result = and tests

tests :: [Bool]
tests =
  [ I.run example1 == Just (IntegerVal 5)
  , I.run example2 == Just (IntegerVal 10)
  , I.run example3 == Just (IntegerVal 56)
  , I.run example4 == Just (BoolVal False)
  , I.run example5 == Just (BoolVal True)
  , I.run example6 == Just (IntegerVal 2)
  , I.run example7 == Just (IntegerVal 3)
  , I.run example8 == Just (IntegerVal (-5))
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
