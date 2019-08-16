-- A ribcage representation for environments.

newtype Env k v = Env [([k], [v])]

empty :: Env k v
empty = Env []

extend :: k -> v -> Env k v -> Env k v
extend k v = extendMany [k] [v]

apply :: Eq k => Env k v -> k -> Maybe v
apply (Env bs) k = lookup bs
  where
    lookup [] = Nothing
    lookup ((ks, vs):rest) =
      case findKey ks vs of
        Nothing -> lookup rest
        Just v -> Just v

    findKey [] _ = Nothing
    findKey _ [] = Nothing
    findKey (k':ks) (v:vs)
      | k == k' = Just v
      | otherwise = findKey ks vs

-- Exercise 2.8
isEmpty :: Env k v -> Bool
isEmpty (Env []) = True
isEmpty _ = False

-- Exercise 2.9
hasBinding :: Eq k => Env k v -> k -> Bool
hasBinding (Env bs) k = or [k `elem` ks | (ks, _) <- bs]

-- Exercise 2.10
extendMany :: [k] -> [v] -> Env k v -> Env k v
extendMany ks vs (Env bs) = Env ((ks, vs) : bs)

-- Usage

e :: Env Char Int
e = extendMany ['d', 'y', 'x', 'y'] [6, 8, 7, 14] empty

ee :: Env Char Int
ee =
  extendMany ['a', 'b', 'c'] [11, 12, 13] $
    extendMany ['x', 'z'] [66, 77] $
      extendMany ['x', 'y'] [88, 99] $
        empty

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

  , not (isEmpty ee)

  , hasBinding ee 'a'
  , hasBinding ee 'b'
  , hasBinding ee 'c'
  , hasBinding ee 'x'
  , hasBinding ee 'y'
  , hasBinding ee 'z'
  , not (hasBinding ee 'd')

  , apply ee 'a' == Just 11
  , apply ee 'b' == Just 12
  , apply ee 'c' == Just 13
  , apply ee 'x' == Just 66
  , apply ee 'y' == Just 99
  , apply ee 'z' == Just 77
  ]

result = and tests
