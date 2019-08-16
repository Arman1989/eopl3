module AST (Program(..), Expr(..), Decl(..), Id) where

data Program = Program Expr deriving Show

data Expr
  = Const Integer
  | Var Id
  | Begin [Expr]
  | Diff Expr Expr
  | Zero Expr
  | If Expr Expr Expr
  | Let Id Expr Expr
  | Letrec [Decl] Expr
  | Proc Id Expr
  | Call Expr Expr
  | Newref Expr
  | Deref Expr
  | Setref Expr Expr
  deriving Show

data Decl = Decl Id Id Expr deriving Show

type Id = String
