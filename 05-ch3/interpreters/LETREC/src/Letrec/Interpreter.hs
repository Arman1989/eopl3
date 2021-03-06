module Letrec.Interpreter (Value, run) where

import qualified Letrec.Env as Env

import Letrec.AST
import Letrec.Parser (parse)

data Value
  = NumberVal Number
  | BoolVal Bool
  | ProcedureVal Procedure

type Environment = Env.Env Id Value Expr

instance Show Value where
  show (NumberVal n) = show n
  show (BoolVal b) = if b then "True" else "False"
  show (ProcedureVal _) = "<<proc>>"

run :: String -> Value
run = valueOfProgram . parse

valueOfProgram :: Program -> Value
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv
  where
    initEnv =
      Env.extend "i" (NumberVal 1)
        (Env.extend "v" (NumberVal 5)
          (Env.extend "x" (NumberVal 10)
            Env.empty))

valueOfExpr :: Expr -> Environment -> Value
valueOfExpr expr env =
  case expr of
    Const n ->
      NumberVal n

    Var v ->
      Env.apply env v procedureVal

    Diff a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        NumberVal (toNumber aVal - toNumber bVal)

    Zero e ->
      let
        val = valueOfExpr e env
      in
        BoolVal (toNumber val == 0)

    If test consequent alternative ->
      let
        testVal = valueOfExpr test env
      in
        if (toBool testVal) then
          valueOfExpr consequent env
        else
          valueOfExpr alternative env

    Let var e body ->
      let
        val = valueOfExpr e env
      in
        valueOfExpr body (Env.extend var val env)

    Proc var body ->
      procedureVal var body env

    Call f arg ->
      let
        fVal = valueOfExpr f env
        argVal = valueOfExpr arg env
      in
        applyProcedure (toProcedure fVal) argVal

    Letrec name param body e ->
      valueOfExpr e (Env.extendRec name param body env)

toNumber :: Value -> Number
toNumber (NumberVal n) = n
toNumber x = error ("Expected a number: " ++ show x)

toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool x = error ("Expected a boolean: " ++ show x)

toProcedure :: Value -> Procedure
toProcedure (ProcedureVal p) = p
toProcedure x = error ("Expected a procedure: " ++ show x)

-- Procedure ADT

data Procedure = Procedure Id Expr Environment

procedure :: Id -> Expr -> Environment -> Procedure
procedure = Procedure

procedureVal :: Id -> Expr -> Environment -> Value
procedureVal var body env =
  ProcedureVal (procedure var body env)

applyProcedure :: Procedure -> Value -> Value
applyProcedure (Procedure var body env) val =
  valueOfExpr body (Env.extend var val env)
