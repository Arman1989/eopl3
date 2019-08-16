-- A procedural representation for environments.

newtype Env k v = Env (k -> Maybe v)

empty :: Env k v
empty = Env (\_ -> Nothing)

extend :: Eq k => k -> v -> Env k v -> Env k v
extend k v (Env e) = Env (\k' -> if k' == k then Just v else e k')

apply :: Env k v -> k -> Maybe v
apply (Env e) = e

-- Usage

e :: Env Char Int
e =
  extend 'd' 6 $
    extend 'y' 8 $
      extend 'x' 7 $
        extend 'y' 14 $
          empty

-- Tests

tests =
  [ apply e 'd' == Just 6
  , apply e 'y' == Just 8
  , apply e 'x' == Just 7
  , apply e 'z' == Nothing
  ]

result = and tests
