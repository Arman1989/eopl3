module AST (Program(..), Expr(..), Id) where

data Program = Program Expr deriving Show

data Expr
  = Const Integer
  | Var Id
  | Diff Expr Expr
  | Minus Expr
  | Add Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Zero Expr
  | Equal Expr Expr
  | Greater Expr Expr
  | Less Expr Expr
  | Cons Expr Expr
  | Car Expr
  | Cdr Expr
  | Null Expr
  | EmptyList
  | If Expr Expr Expr
  | Let Id Expr Expr
  deriving Show

type Id = String
