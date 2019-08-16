module Interpreter (Value(..), run) where

import Data.Maybe (fromJust)
import Prelude hiding (toInteger)

import AST
import Env
import Parser

data Value
  = IntegerVal Integer
  | BoolVal Bool
  | ClosureVal Closure

data Closure = Closure Id Expr Environment

type Environment = Env Id Value

instance Show Value where
  show (IntegerVal i) = show i
  show (BoolVal b) = if b then "#t" else "#f"
  show (ClosureVal _) = "<<closure>>"

run :: String -> Value
run = valueOfProgram . fromJust . Parser.parse

valueOfProgram :: Program -> Value
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv

valueOfExpr :: Expr -> Environment -> Value
valueOfExpr expr env =
  case expr of
    Const n ->
      IntegerVal n

    Var x ->
      Env.apply env x

    Diff a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        IntegerVal ((toInteger aVal) - (toInteger bVal))

    Zero e ->
      let
        val = valueOfExpr e env
      in
        BoolVal (toInteger val == 0)

    If test consequent alternative ->
      let
        testVal = valueOfExpr test env
      in
        if (toBool testVal) then
          valueOfExpr consequent env
        else
          valueOfExpr alternative env

    Let id e body ->
      let
        val = valueOfExpr e env
      in
        valueOfExpr body (Env.extend id val env)

    Proc id body ->
      ClosureVal (Closure id body env)

    Call f arg ->
      let
        fVal = valueOfExpr f env
        argVal = valueOfExpr arg env
      in
        applyClosure (toClosure fVal) argVal

toInteger :: Value -> Integer
toInteger (IntegerVal n) = n
toInteger _ = error "Expected an integer"

toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool _ = error "Expected a boolean"

toClosure :: Value -> Closure
toClosure (ClosureVal c) = c
toClosure _ = error "Expected a procedure"

applyClosure :: Closure -> Value -> Value
applyClosure (Closure id body savedEnv) val =
  valueOfExpr body (Env.extend id val savedEnv)

initEnv :: Environment
initEnv =
  Env.extend "i" (IntegerVal 1)
    (Env.extend "v" (IntegerVal 5)
      (Env.extend "x" (IntegerVal 10)
        Env.empty))
