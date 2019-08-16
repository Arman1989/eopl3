module Interpreter (Value(..), run) where

import Data.Maybe (fromJust)
import Prelude hiding (toInteger)

import AST
import Env
import Parser
import Translator

data Value
  = IntegerVal Integer
  | BoolVal Bool
  | ClosureVal Closure

data Closure = Closure NamelessExpr Environment

type Environment = Env Value

instance Show Value where
  show (IntegerVal i) = show i
  show (BoolVal b) = if b then "#t" else "#f"
  show (ClosureVal _) = "<<closure>>"

run :: String -> Value
run = valueOfNamelessProgram . Translator.translate . fromJust . Parser.parse

valueOfNamelessProgram :: NamelessProgram -> Value
valueOfNamelessProgram (Program expr) =
  valueOfNamelessExpr expr initEnv

valueOfNamelessExpr :: NamelessExpr -> Environment -> Value
valueOfNamelessExpr expr env =
  case expr of
    Const n ->
      IntegerVal n

    Diff a b ->
      let
        aVal = valueOfNamelessExpr a env
        bVal = valueOfNamelessExpr b env
      in
        IntegerVal ((toInteger aVal) - (toInteger bVal))

    Zero e ->
      let
        val = valueOfNamelessExpr e env
      in
        BoolVal (toInteger val == 0)

    If test consequent alternative ->
      let
        testVal = valueOfNamelessExpr test env
      in
        if (toBool testVal) then
          valueOfNamelessExpr consequent env
        else
          valueOfNamelessExpr alternative env

    Call f arg ->
      let
        fVal = valueOfNamelessExpr f env
        argVal = valueOfNamelessExpr arg env
      in
        applyClosure (toClosure fVal) argVal

    NamelessVar n ->
      Env.apply env n

    NamelessLet e body ->
      let
        val = valueOfNamelessExpr e env
      in
        valueOfNamelessExpr body (Env.extend val env)

    NamelessProc body ->
      ClosureVal (Closure body env)

    _ ->
      error "Invalid translated expression"

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
applyClosure (Closure body savedEnv) val =
  valueOfNamelessExpr body (Env.extend val savedEnv)

initEnv :: Environment
initEnv =
  Env.extend (IntegerVal 1)
    (Env.extend (IntegerVal 5)
      (Env.extend (IntegerVal 10)
        Env.empty))
