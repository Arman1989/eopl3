module Data.Natural.Bignum (Bignum) where

-- Natural numbers are represented in base B, for some large integer B. The
-- representation is as a list of numbers between 0 and B-1 (called bigits).
-- The least-significant bigit is kept at the head of the list.

import Data.Natural hiding ((+), (*))

newtype Bignum = Bignum [Int]

instance Show Bignum where
  show (Bignum ns) = "Bignum " ++ show (toDecimal ns)

instance Natural Bignum where
  zero = Bignum []

  isZero (Bignum []) = True
  isZero _ = False

  succ (Bignum ns) = Bignum (add1 ns)

  pred (Bignum []) = error "Data.Natural.Bignum.pred: 0 has no predecessor"
  pred (Bignum ns) = Bignum (sub1 ns)

baseN :: Num a => a
baseN = 1024

add1 :: (Eq a, Num a) => [a] -> [a]
add1 [] = [1]
add1 (n:ns)
  | d == baseN = 0 : add1 ns
  | otherwise  = d : ns
  where
    d = n+1

sub1 :: (Eq a, Num a) => [a] -> [a]
sub1 (0:ns) = (baseN-1) : sub1 ns
sub1 (1:[]) = []
sub1 (n:ns) = (n-1) : ns

toDecimal :: Integral a => [a] -> Integer
toDecimal = toBase10 1 0

toBase10 :: Integral a => Integer -> Integer -> [a] -> Integer
toBase10 _ s [] = s
toBase10 m s (n:ns) = toBase10 (m * baseN) (s + (toInteger n) * m) ns
