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
  , I.run example9 == Just (IntegerVal 14)
  , I.run example10 == Just (IntegerVal 5)
  , I.run example11 == Just (BoolVal True)
  , I.run example12 == Just (BoolVal False)
  , I.run example13 == Just (IntegerVal 10)
  , I.run example14 == Just
      (ListVal (Cons (IntegerVal 4)
                     (ListVal (Cons (ListVal (Cons (IntegerVal 3)
                                                   (ListVal Empty)))
                                    (ListVal Empty)))))
  , I.run example15 == Just (ListVal Empty)
  , I.run example16 == Just (ListVal (Cons (IntegerVal 1)
                                           (ListVal (Cons (IntegerVal 2)
                                                          (IntegerVal 3)))))
  , I.run example17 == Just (IntegerVal 1)
  , I.run example18 == Just (IntegerVal 2)
  , I.run example19 == Just (BoolVal True)
  , I.run example20 == Just (BoolVal False)
  , I.run example21 == Just (IntegerVal 1)
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
example9 = "minus(-(minus(5), 9))"

example10 :: String
example10 = "div(add(96, -(5, 1)), mult(2, 10))"

example11 :: String
example11 = "equal?(add(5, 5), mult(2, 5))"

example12 :: String
example12 = "greater?(let x = 3 in div(300, x), 101)"

example13 :: String
example13 = "if less?(-(1, 5), mult(2, 2)) then 10 else 20"

example14 :: String
example14 = "                    \
  \ let x = 4 in                 \
  \   cons(x,                    \
  \        cons(cons(-(x, 1),    \
  \                  emptylist), \
  \             emptylist))      "

example15 :: String
example15 = "emptylist"

example16 :: String
example16 = "cons(1, cons(2, 3))"

example17 :: String
example17 = "car(cons(1, 2))"

example18 :: String
example18 = "cdr(cons(1, 2))"

example19 :: String
example19 = "null?(emptylist)"

example20 :: String
example20 = "null?(cons(emptylist, emptylist))"

example21 :: String
example21 = "                     \
  \ let l = cons(1, emptylist) in \
  \   if null?(l) then            \
  \     0                         \
  \   else                        \
  \     car(l)                    "
