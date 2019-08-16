module Data.Natural.Haskell (Haskell) where

-- A Haskell integer representation. We use Haskell's internal representation
-- of arbitrary precision integers.

import Data.Natural hiding ((+))

newtype Haskell = Haskell Integer

instance Show Haskell where
  show (Haskell n) = "Haskell " ++ show n

instance Natural Haskell where
  zero = Haskell 0

  isZero (Haskell 0) = True
  isZero _ = False

  succ (Haskell n) = Haskell (n + 1)

  pred (Haskell 0) = error "Data.Natural.Haskell.pred: 0 has no predecessor"
  pred (Haskell n) = Haskell (n - 1)
