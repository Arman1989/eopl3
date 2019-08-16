import Data.Maybe (isNothing)

-- A procedural representation for environments.

data Env k v = Env
  { _apply :: k -> Maybe v
  , _isEmpty :: () -> Bool
  , _hasBinding :: k -> Bool
  }

empty :: Env k v
empty = Env
  { _apply = \_ -> Nothing
  , _isEmpty = \_ -> True
  , _hasBinding = \_ -> False
  }

extend :: Eq k => k -> v -> Env k v -> Env k v
extend k v env = Env
  { _apply = \k' -> if k' == k then Just v else (_apply env) k'
  , _isEmpty = \_ -> False
  , _hasBinding = \k' -> if k' == k then True else (_hasBinding env) k'
  }

apply :: Env k v -> k -> Maybe v
apply env = _apply env

isEmpty :: Env k v -> Bool
isEmpty env = (_isEmpty env) ()

hasBinding :: Env k v -> k -> Bool
hasBinding env = _hasBinding env

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
  [ isEmpty empty
  , not (isEmpty e)

  , hasBinding e 'd'
  , hasBinding e 'y'
  , hasBinding e 'x'
  , not (hasBinding e 'z')

  , apply e 'd' == Just 6
  , apply e 'y' == Just 8
  , apply e 'x' == Just 7
  , isNothing (apply e 'z')
  ]

result = and tests
