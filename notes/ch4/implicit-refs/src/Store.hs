module Store (Store, Ref, empty, newref, deref, setref) where

data Store a = Store [a]

data Ref = Ref Int

instance Show Ref where
  show (Ref r) = show r

empty :: Store a
empty = Store []

newref :: a -> Store a -> (Ref, Store a)
newref v (Store vs) = (Ref nextRef, Store (vs ++ [v]))
  where
    nextRef = length vs

deref :: Ref -> Store a -> a
deref (Ref ref) (Store values) = index ref values
  where
    index _ [] = error ("Invalid reference: " ++ show ref)
    index r (v:vs)
      | r == 0 = v
      | otherwise = index (r-1) vs

setref :: Ref -> a -> Store a -> Store a
setref (Ref ref) value (Store values) = Store (update ref values)
  where
    update _ [] = error ("Invalid reference: " ++ show ref)
    update r (v:vs)
      | r == 0 = value : vs
      | otherwise = v : update (r-1) vs
