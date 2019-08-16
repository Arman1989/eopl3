module Data.Natural
  ( Natural(zero, isZero, succ, pred)
  , (+), (*)
  , fact
  ) where

import Prelude hiding ((+), (*), pred, succ)

class Natural a where
  zero :: a
  isZero :: a -> Bool
  succ :: a -> a
  pred :: a -> a

infixl 7 *
infixl 6 +

(+) :: Natural a => a -> a -> a
x + y
  | isZero x = y
  | otherwise = succ (pred x + y)

(*) :: Natural a => a -> a -> a
x * y
  | isZero x = zero
  | otherwise = y + (pred x * y)

fact :: Natural a => a -> a
fact = iter one
  where
    iter result n
      | isZero n = result
      | otherwise = iter (n * result) (pred n)

one :: Natural a => a
one = succ zero
