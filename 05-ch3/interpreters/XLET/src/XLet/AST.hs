module XLet.AST (Program(..), Expr(..), Number, Id) where

data Program = Program Expr deriving Show

data Expr
  = Const Number
  | Var Id
  | Diff Expr Expr
  | Zero Expr
  | If Expr Expr Expr
  | Let Id Expr Expr
  | Minus Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Equal Expr Expr
  | Greater Expr Expr
  | Less Expr Expr
  | Cons Expr Expr
  | Car Expr
  | Cdr Expr
  | Null Expr
  | EmptyList
  deriving Show

type Number = Integer

type Id = String
