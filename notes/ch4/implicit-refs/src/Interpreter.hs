module Interpreter (EValue(..), run) where

import Data.Maybe (fromJust)
import Prelude hiding (init, toInteger)

import AST
import Env
import Parser
import qualified Store

data EValue
  = IntegerVal Integer
  | BoolVal Bool
  | ClosureVal Closure

type DValue = Store.Ref

data Closure = Closure Id Expr Environment

type Environment = Env Id DValue
type Store = Store.Store EValue

instance Show EValue where
  show (IntegerVal i) = show i
  show (BoolVal b) = if b then "#t" else "#f"
  show (ClosureVal _) = "<<closure>>"

run :: String -> EValue
run = valueOfProgram . fromJust . Parser.parse

valueOfProgram :: Program -> EValue
valueOfProgram (Program expr) =
  let
    (initEnv, initStore) = init
  in
    fst (valueOfExpr expr initEnv initStore)

valueOfExpr :: Expr -> Environment -> Store -> (EValue, Store)
valueOfExpr expr env store =
  case expr of
    Const n ->
      (IntegerVal n, store)

    Var x ->
      (Store.deref (Env.apply env x) store, store)

    Begin es ->
      let
        doEach [e] s = valueOfExpr e env s
        doEach (e:es) s = doEach es (snd (valueOfExpr e env s))
      in
        doEach es store

    Assign id e ->
      let
        (val, store') = valueOfExpr e env store
      in
        (val, Store.setref (Env.apply env id) val store')

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
        (ref, store'') = Store.newref val store'
      in
        valueOfExpr body (Env.extend id ref env) store''

    Letrec decls e ->
      -- TODO: Figure out how to find the value of a multideclaration letrec.
      undefined

    Proc id body ->
      (ClosureVal (Closure id body env), store)

    Call f arg ->
      let
        (fVal, store') = valueOfExpr f env store
        (argVal, store'') = valueOfExpr arg env store'
      in
        applyClosure (toClosure fVal) argVal store''

toInteger :: EValue -> Integer
toInteger (IntegerVal n) = n
toInteger _ = error "Expected an integer"

toBool :: EValue -> Bool
toBool (BoolVal b) = b
toBool _ = error "Expected a boolean"

toClosure :: EValue -> Closure
toClosure (ClosureVal c) = c
toClosure _ = error "Expected a procedure"

applyClosure :: Closure -> EValue -> Store -> (EValue, Store)
applyClosure (Closure id body savedEnv) val store =
  let
    (ref, store') = Store.newref val store
  in
    valueOfExpr body (Env.extend id ref savedEnv) store'

init :: (Environment, Store)
init =
  let
    (iRef, store) = Store.newref (IntegerVal 1) Store.empty
    (vRef, store') = Store.newref (IntegerVal 5) store
    (xRef, store'') = Store.newref (IntegerVal 10) store'
    env =
      Env.extend "x" xRef
        (Env.extend "v" vRef
          (Env.extend "i" iRef
            Env.empty))
  in
    (env, store'')
