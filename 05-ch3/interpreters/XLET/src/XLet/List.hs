module XLet.List (List, empty, isEmpty, cons, car, cdr) where

data List a
  = Empty
  | Cons a a

empty :: List a
empty = Empty

isEmpty :: List a -> Bool
isEmpty Empty = True
isEmpty (Cons _ _) = False

cons :: a -> a -> List a
cons = Cons

car :: List a -> a
car Empty = error "Cannot return car of an empty list"
car (Cons x _) = x

cdr :: List a -> a
cdr Empty = error "Cannot return cdr of an empty list"
cdr (Cons _ x) = x
