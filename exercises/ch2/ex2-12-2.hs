import Data.Maybe (fromJust, isNothing)

-- A procedural representation of stacks.
--
-- I assume that pop, top and isEmpty are the observers.

data Stack a = Stack
  { _pop :: () -> Maybe (Stack a)
  , _top :: () -> Maybe a
  , _isEmpty :: () -> Bool
  }

empty :: Stack a
empty = Stack
  { _pop = \() -> Nothing
  , _top = \() -> Nothing
  , _isEmpty = \() -> True
  }

push :: a -> Stack a -> Stack a
push x s = Stack
  { _pop = \() -> Just s
  , _top = \() -> Just x
  , _isEmpty = \() -> False
  }

pop :: Stack a -> Maybe (Stack a)
pop s = (_pop s) ()

top :: Stack a -> Maybe a
top s = (_top s) ()

isEmpty :: Stack a -> Bool
isEmpty s = (_isEmpty s) ()

-- Usage

s :: Stack Int
s = push 5 $ push 4 $ push 3 $ push 2 $ push 1 empty

t :: Stack Int
t = fromJust (pop s)

-- Tests

tests =
  [ isEmpty empty
  , not (isEmpty s)

  , isNothing (top empty)
  , top s == Just 5
  , top t == Just 4

  , isNothing (pop empty)
  ]

result = and tests
