module Env (Env, empty, extend, extendRec, apply) where

data Env k v = Env [(k, Value k v)]

data Value k v
  = Value v
  | MakeValue (Env k v -> v)

empty :: Env k v
empty = Env []

extend :: k -> v -> Env k v -> Env k v
extend k v (Env bs) = Env ((k, Value v):bs)

extendRec :: k -> (Env k v -> v) -> Env k v -> Env k v
extendRec k makeValue (Env bs) = Env ((k, MakeValue makeValue):bs)

apply :: (Eq k, Show k) => Env k v -> k -> v
apply (Env bindings) key = lookup bindings
  where
    lookup [] = error ("No binding for " ++ show key)
    lookup bs@((k, v):rest) =
      if k == key then
        case v of
          Value value -> value
          MakeValue makeValue -> makeValue (Env bs)
      else
        lookup rest
