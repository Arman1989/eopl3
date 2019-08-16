module Interpreter (Value(..), List(..), run) where

import Prelude hiding (toInteger)

import AST
import Env
import Parser

data Value
  = IntegerVal Integer
  | BoolVal Bool
  | ListVal List
  deriving (Eq, Show)

data List
  = Empty
  | Cons Value Value
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

    Minus e ->
      let
        val = valueOfExpr e env
      in
        IntegerVal (negate (toInteger val))

    Add a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        IntegerVal ((toInteger aVal) + (toInteger bVal))

    Mult a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        IntegerVal ((toInteger aVal) * (toInteger bVal))

    Div a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        IntegerVal ((toInteger aVal) `div` (toInteger bVal))

    Zero e ->
      let
        val = valueOfExpr e env
      in
        BoolVal (toInteger val == 0)

    Equal a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        BoolVal ((toInteger aVal) == (toInteger bVal))

    Greater a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        BoolVal ((toInteger aVal) > (toInteger bVal))

    Less a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        BoolVal ((toInteger aVal) < (toInteger bVal))

    AST.Cons a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        ListVal (Interpreter.Cons aVal bVal)

    Car l ->
      let
        lVal = valueOfExpr l env
      in
        car (toList lVal)

    Cdr l ->
      let
        lVal = valueOfExpr l env
      in
        cdr (toList lVal)

    Null l ->
      let
        lVal = valueOfExpr l env
      in
        BoolVal (isNull (toList lVal))

    EmptyList ->
      ListVal Empty

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

toList :: Value -> List
toList (ListVal l) = l
toList x = error ("Expected a list: " ++ show x)

car :: List -> Value
car Empty = error "Expected a non-empty list"
car (Interpreter.Cons h _) = h

cdr :: List -> Value
cdr Empty = error "Expected a non-empty list"
cdr (Interpreter.Cons _ t) = t

isNull :: List -> Bool
isNull Empty = True
isNull _ = False

initEnv :: Environment
initEnv =
  Env.extend "i" (IntegerVal 1)
    (Env.extend "v" (IntegerVal 5)
      (Env.extend "x" (IntegerVal 10)
        Env.empty))
