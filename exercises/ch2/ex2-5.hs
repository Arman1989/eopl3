-- A representation for environments using a-list or association-list.

newtype Env k v = Env [(k, v)]

empty :: Env k v
empty = Env []

extend :: k -> v -> Env k v -> Env k v
extend k v (Env bs) = Env ((k, v) : bs)

apply :: Eq k => Env k v -> k -> Maybe v
apply (Env bs) k = lookup k bs

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
