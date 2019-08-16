inS :: Int -> Bool
inS n
  | n == 0 = True
  | n >= 3 = inS (n-3) -- n-3 >= 0 => n >= 3
  | otherwise = False

-- Alternatively,

inS' :: Int -> Bool
inS' n = n == 0 || (n >= 3 && inS' (n-3))

-- The following lists contain only multiples of 3
a = take 50 [n | n <- [-100..], inS n]
b = take 50 [n | n <- [-100..], inS' n]
