module Data.Natural.Unary (Unary) where

-- A unary representation. The natural number, n, is represented by
-- a list of n True's.

import Data.Natural

newtype Unary = Unary [Bool]

instance Show Unary where
  show (Unary ts) = "Unary " ++ show (length ts)

instance Natural Unary where
  zero = Unary []

  isZero (Unary []) = True
  isZero _ = False

  succ (Unary ts) = Unary (True : ts)

  pred (Unary []) = error "Data.Natural.Unary.pred: 0 has no predecessor"
  pred (Unary (_:ts)) = Unary ts
