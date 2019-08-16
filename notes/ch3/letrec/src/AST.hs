module AST (Program(..), Expr(..), Id) where

data Program = Program Expr deriving Show

data Expr
  = Const Integer
  | Var Id
  | Diff Expr Expr
  | Zero Expr
  | If Expr Expr Expr
  | Let Id Expr Expr
  | Letrec Id Id Expr Expr
  | Proc Id Expr
  | Call Expr Expr
  deriving Show

type Id = String