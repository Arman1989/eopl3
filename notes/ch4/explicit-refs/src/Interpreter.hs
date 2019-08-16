module Interpreter (Value(..), run) where

import Data.Maybe (fromJust)
import Prelude hiding (toInteger)

import AST
import Env
import Parser
import Store

data Value
  = IntegerVal Integer
  | BoolVal Bool
  | ClosureVal Closure
  | RefVal Ref

data Closure = Closure Id Expr Environment

type Environment = Env Id Value

instance Show Value where
  show (IntegerVal i) = show i
  show (BoolVal b) = if b then "#t" else "#f"
  show (ClosureVal _) = "<<closure>>"
  show (RefVal r) = "<<reference:" ++ show r ++ ">>"

run :: String -> Value
run = valueOfProgram . fromJust . Parser.parse

valueOfProgram :: Program -> Value
valueOfProgram (Program expr) =
  fst (valueOfExpr expr initEnv initStore)

valueOfExpr :: Expr -> Environment -> Store Value -> (Value, Store Value)
valueOfExpr expr env store =
  case expr of
    Const n ->
      (IntegerVal n, store)

    Var x ->
      (Env.apply env x, store)

    Begin es ->
      let
        doEach [e] s = valueOfExpr e env s
        doEach (e:es) s = doEach es (snd (valueOfExpr e env s))
      in
        doEach es store

    Diff a b ->
      let
        (aVal, store') = valueOfExpr a env store
        (bVal, store'') = valueOfExpr b env store'
      in
        (IntegerVal ((toInteger aVal) - (toInteger bVal)), store'')

    Zero e ->
      let
        (val, store') = valueOfExpr e env store
      in
        (BoolVal (toInteger val == 0), store')

    If test consequent alternative ->
      let
        (testVal, store') = valueOfExpr test env store
      in
        if (toBool testVal) then
          valueOfExpr consequent env store'
        else
          valueOfExpr alternative env store'

    Let id e body ->
      let
        (val, store') = valueOfExpr e env store
      in
        valueOfExpr body (Env.extend id val env) store'

    Letrec decls e ->
      let
        (names, makeClosures) = prepare decls
      in
        valueOfExpr e (Env.extendRec names makeClosures env) store

    Proc id body ->
      (ClosureVal (Closure id body env), store)

    Call f arg ->
      let
        (fVal, store') = valueOfExpr f env store
        (argVal, store'') = valueOfExpr arg env store'
      in
        applyClosure (toClosure fVal) argVal store''

    Newref e ->
      let
        (val, store') = valueOfExpr e env store
        (ref, store'') = Store.newref val store'
      in
        (RefVal ref, store'')

    Deref e ->
      let
        (refVal, store') = valueOfExpr e env store
      in
        (Store.deref (toRef refVal) store', store')

    Setref r e ->
      let
        (refVal, store') = valueOfExpr r env store
        (val, store'') = valueOfExpr e env store'
      in
        (val, Store.setref (toRef refVal) val store'')

toInteger :: Value -> Integer
toInteger (IntegerVal n) = n
toInteger _ = error "Expected an integer"

toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool _ = error "Expected a boolean"

toClosure :: Value -> Closure
toClosure (ClosureVal c) = c
toClosure _ = error "Expected a procedure"

toRef :: Value -> Ref
toRef (RefVal r) = r
toRef _ = error "Expected a reference"

applyClosure :: Closure -> Value -> Store Value -> (Value, Store Value)
applyClosure (Closure id body savedEnv) val store =
  valueOfExpr body (Env.extend id val savedEnv) store

prepare :: [Decl] -> ([Id], [Environment -> Value])
prepare [] = ([], [])
prepare ((Decl name id body):ds) =
  let
    (names, makeClosures) = prepare ds
  in
    (name:names, (\env' -> ClosureVal (Closure id body env')):makeClosures)

initEnv :: Environment
initEnv =
  Env.extend "i" (IntegerVal 1)
    (Env.extend "v" (IntegerVal 5)
      (Env.extend "x" (IntegerVal 10)
        Env.empty))

initStore :: Store Value
initStore =
  Store.empty
