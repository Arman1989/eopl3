module StaticEnv (StaticEnv, empty, extend, apply) where

import AST (Lexaddr)

data StaticEnv a = StaticEnv [a]

empty :: StaticEnv a
empty = StaticEnv []

extend :: a -> StaticEnv a -> StaticEnv a
extend var (StaticEnv vars) = StaticEnv (var:vars)

apply :: (Eq a, Show a) => StaticEnv a -> a -> Lexaddr
apply (StaticEnv vars) var = indexOf vars 0
  where
    indexOf [] _ = error ("No binding for " ++ show var)
    indexOf (v:vs) i
      | v == var = i
      | otherwise = indexOf vs (i+1)
