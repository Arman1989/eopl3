type Id = String

data Expr
  = Var Id
  | Lambda Id Expr
  | App Expr Expr

instance Show Expr where
  show (Var id) = id
  show (Lambda id body) = "(λ" ++ id ++ "." ++ show body ++ ")"
  show (App rator rand) = "(" ++ show rator ++ " " ++ show rand ++ ")"

occursFree :: Id -> Expr -> Bool
occursFree x (Var id) = x == id
occursFree x (Lambda id body) = x /= id && occursFree x body
occursFree x (App rator rand) = occursFree x rator || occursFree x rand

-- Tests

e1 :: Expr
e1 = Var "a"

e2 :: Expr
e2 = Lambda "a" (Var "b")

e3 :: Expr
e3 = Lambda "a" (Var "a")

e4 :: Expr
e4 = App (Lambda "a" (Var "a")) (Var "b")

tests =
  [ occursFree "a" e1 == True
  , occursFree "b" e1 == False

  , occursFree "a" e2 == False
  , occursFree "b" e2 == True

  , occursFree "a" e3 == False
  , occursFree "b" e3 == False

  , occursFree "a" e4 == False
  , occursFree "b" e4 == True
  ]

result = and tests
