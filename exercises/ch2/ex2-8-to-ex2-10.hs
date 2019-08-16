-- A representation for environments using a-list or association-list.

newtype Env k v = Env [(k, v)]

empty :: Env k v
empty = Env []

extend :: k -> v -> Env k v -> Env k v
extend k v (Env bs) = Env ((k, v) : bs)

apply :: Eq k => Env k v -> k -> Maybe v
apply (Env bs) k = lookup k bs

-- Exercise 2.8
isEmpty :: Env k v -> Bool
isEmpty (Env []) = True
isEmpty _ = False

-- Exercise 2.9
hasBinding :: Eq k => Env k v -> k -> Bool
hasBinding (Env bs) k = any (((==) k) . fst) bs

-- Exercise 2.10
extendMany :: [k] -> [v] -> Env k v -> Env k v
extendMany ks vs (Env bs) = Env (zip ks vs ++ bs)

-- Usage

e :: Env Char Int
e = extendMany ['d', 'y', 'x', 'y'] [6, 8, 7, 14] empty

-- Tests

tests =
  [ isEmpty empty
  , not (isEmpty e)

  , hasBinding e 'd'
  , hasBinding e 'y'
  , hasBinding e 'x'
  , not (hasBinding e 'z')

  , apply e 'd' == Just 6
  , apply e 'y' == Just 8
  , apply e 'x' == Just 7
  , apply e 'z' == Nothing
  ]

result = and tests
