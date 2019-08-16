module Test where

import Prelude hiding ((+), (*), succ, pred)

import Data.Natural
import Data.Natural.Bignum
import Data.Natural.Haskell
import Data.Natural.Unary

ten :: Natural a => a
ten = succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero)))))))))

x :: Unary
x = fact ten

y :: Haskell
y = fact ten

z :: Bignum
z = fact ten

-- Further questions to be answered in ex 2.1:
-- 1. How does the execution time of `fact` vary as the argument changes?
-- 2. How does the execution time of `fact` vary as the base of Bignum changes?
