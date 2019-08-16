module Env (Env, empty, extend, extendRec, apply) where

data Env k v = Env [([k], Value k v)]

data Value k v
  = Value v
  | MakeValue [Env k v -> v]

empty :: Env k v
empty = Env []

extend :: k -> v -> Env k v -> Env k v
extend k v (Env bs) = Env (([k], Value v):bs)

extendRec :: [k] -> [Env k v -> v] -> Env k v -> Env k v
extendRec ks makeValues (Env bs) = Env ((ks, MakeValue makeValues):bs)

apply :: (Eq k, Show k) => Env k v -> k -> v
apply (Env bindings) key = lookup bindings
  where
    lookup [] = error ("No binding for " ++ show key)
    lookup (([k], Value v):rest) =
      if k == key then
        v
      else
        lookup rest
    lookup bs@((ks, MakeValue makeValues):rest) =
      case lookupMakeValue ks makeValues of
        Nothing -> lookup rest
        Just makeValue -> makeValue (Env bs)

    lookupMakeValue [] [] = Nothing
    lookupMakeValue (k:ks) (makeValue:makeValues) =
      if k == key then
        Just makeValue
      else
        lookupMakeValue ks makeValues
