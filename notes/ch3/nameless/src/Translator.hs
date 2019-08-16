module Translator (NamelessProgram, NamelessExpr, translate) where

import AST
import StaticEnv

type NamelessProgram = Program
type NamelessExpr = Expr

type Senv = StaticEnv Id

translate :: Program -> NamelessProgram
translate (Program expr) =
  Program (translateExpr expr initSenv)

translateExpr :: Expr -> Senv -> NamelessExpr
translateExpr expr senv =
  case expr of
    Const n ->
      Const n

    Var x ->
      NamelessVar (StaticEnv.apply senv x)

    Diff a b ->
      Diff (translateExpr a senv) (translateExpr b senv)

    Zero e ->
      Zero (translateExpr e senv)

    If test consequent alternative ->
      If (translateExpr test senv)
         (translateExpr consequent senv)
         (translateExpr alternative senv)

    Let id e body ->
      NamelessLet (translateExpr e senv)
                  (translateExpr body (StaticEnv.extend id senv))

    Proc id body ->
      NamelessProc (translateExpr body (StaticEnv.extend id senv))

    Call f arg ->
      Call (translateExpr f senv) (translateExpr arg senv)

    _ ->
      error "Invalid source expression"

initSenv :: Senv
initSenv =
  StaticEnv.extend "i"
    (StaticEnv.extend "v"
      (StaticEnv.extend "x"
        StaticEnv.empty))
