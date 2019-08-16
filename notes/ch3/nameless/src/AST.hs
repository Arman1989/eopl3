module AST (Program(..), Expr(..), Id, Lexaddr) where

data Program = Program Expr deriving Show

data Expr
  -- For the source language
  = Const Integer
  | Var Id
  | Diff Expr Expr
  | Zero Expr
  | If Expr Expr Expr
  | Let Id Expr Expr
  | Proc Id Expr
  | Call Expr Expr

  -- For the target language
  | NamelessVar Lexaddr
  | NamelessLet Expr Expr
  | NamelessProc Expr

  deriving Show

type Id = String
type Lexaddr = Int
