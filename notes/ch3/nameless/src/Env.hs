module Env (Env, empty, extend, apply) where

import AST (Lexaddr)

data Env a = Env [a]

empty :: Env a
empty = Env []

extend :: a -> Env a -> Env a
extend value (Env values) = Env (value:values)

apply :: Env a -> Lexaddr -> a
apply (Env values) n = lookup values n
  where
    lookup [] _ = error ("Index out of bounds: " ++ show n)
    lookup (v:vs) i
      | i == 0 = v
      | otherwise = lookup vs (i-1)
