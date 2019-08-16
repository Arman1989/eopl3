module Interpreter (Value(..), run) where

import Prelude hiding (toInteger)

import AST
import Env
import Parser

data Value
  = IntegerVal Integer
  | BoolVal Bool
  deriving (Eq, Show)

type Environment = Env Id Value

run :: String -> Maybe Value
run input =
  valueOfProgram <$> Parser.parse input

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

toInteger :: Value -> Integer
toInteger (IntegerVal n) = n
toInteger x = error ("Expected an integer: " ++ show x)

toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool x = error ("Expected a boolean: " ++ show x)

initEnv :: Environment
initEnv =
  Env.extend "i" (IntegerVal 1)
    (Env.extend "v" (IntegerVal 5)
      (Env.extend "x" (IntegerVal 10)
        Env.empty))
