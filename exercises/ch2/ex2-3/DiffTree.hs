module DiffTree where

import Prelude hiding ((+), pred, succ, toInteger)

-- Exercise 2.3

data DiffTree
  = One
  | Diff DiffTree DiffTree

-- 1. Show that every number has infinitely many representations in this system.
--
-- Let n be any integer and [n] be a representation of n as a diff-tree. Then,
-- n = n - 0 and it follows that Diff [n] (Diff One One) is another
-- representation of n. We can continue to subtract 0 ad infinitum
-- and continue to get distinct representations for n.
--
-- n = n - 0 : Diff [n] (Diff One One)
--   = (n - 0) - 0 : Diff (Diff [n] (Diff One One)) (Diff One One)
--   = ((n - 0) - 0) - 0 : Diff (Diff (Diff [n] (Diff One One)) (Diff One One))
--                              (Diff One One)
--   = ...

instance Show DiffTree where
  show = show . toInteger

toInteger :: DiffTree -> Integer
toInteger One = 1
toInteger (Diff t1 t2) = (toInteger t1) - (toInteger t2)

-- 2. The integers, Z.

newtype Z = Z DiffTree deriving Show

zero :: Z
zero = Z (Diff One One)

isZero :: Z -> Bool
isZero (Z t) = toInteger t == 0

succ :: Z -> Z
succ (Z t) =
  -- n + 1 = n - (-1)
  Z (Diff t (Diff t0 One))

pred :: Z -> Z
pred (Z t) =
  -- n - 1
  Z (Diff t One)

-- 3. Addition.

infixl 6 +

(+) :: Z -> Z -> Z
(Z t1) + (Z t2) =
  -- m + n = m - (-n)
  Z (Diff t1 (Diff t0 t2))

t0 :: DiffTree
t0 = Diff One One
