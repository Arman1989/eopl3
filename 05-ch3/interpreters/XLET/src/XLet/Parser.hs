module XLet.Parser where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Token as Token

import Text.Parsec ((<|>), char, eof, oneOf)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (LanguageDef, TokenParser)

import XLet.AST

parse :: String -> Program
parse input =
  case Parsec.parse program "" input of
    Left err ->
      error (show err)

    Right p ->
      p

-- Non-terminals

program :: Parser Program
program = Program <$> (whiteSpace *> expr <* eof)

expr :: Parser Expr
expr
  = constExpr
  <|> diffExpr
  <|> zeroExpr
  <|> ifExpr
  <|> letExpr
  <|> minusExpr
  <|> addExpr
  <|> mulExpr
  <|> divExpr
  <|> equalExpr
  <|> greaterExpr
  <|> lessExpr
  <|> consExpr
  <|> carExpr
  <|> cdrExpr
  <|> nullExpr
  <|> emptyListExpr
  <|> varExpr

constExpr :: Parser Expr
constExpr = Const <$> number

diffExpr :: Parser Expr
diffExpr = minus *> (parens (Diff <$> (expr <* comma) <*> expr))
  where
    minus = char '-'

zeroExpr :: Parser Expr
zeroExpr = reserved "zero?" *> (parens (Zero <$> expr))

ifExpr :: Parser Expr
ifExpr =
  If <$> (ifToken *> expr) <*> (thenToken *> expr) <*> (elseToken *> expr)
  where
    ifToken = reserved "if"
    thenToken = reserved "then"
    elseToken = reserved "else"

letExpr :: Parser Expr
letExpr =
  Let <$> (letToken *> identifier) <*> (equal *> expr) <*> (inToken *> expr)
  where
    letToken = reserved "let"
    inToken = reserved "in"
    equal = lexeme (char '=')

minusExpr :: Parser Expr
minusExpr = reserved "minus" *> (parens (Minus <$> expr))

addExpr :: Parser Expr
addExpr = addToken *> (parens (Add <$> (expr <* comma) <*> expr))
  where
    addToken = reserved "add"

mulExpr :: Parser Expr
mulExpr = mulToken *> (parens (Mul <$> (expr <* comma) <*> expr))
  where
    mulToken = reserved "mul"

divExpr :: Parser Expr
divExpr = divToken *> (parens (Div <$> (expr <* comma) <*> expr))
  where
    divToken = reserved "div"

equalExpr :: Parser Expr
equalExpr = equalToken *> (parens (Equal <$> (expr <* comma) <*> expr))
  where
    equalToken = reserved "equal?"

greaterExpr :: Parser Expr
greaterExpr = greaterToken *> (parens (Greater <$> (expr <* comma) <*> expr))
  where
    greaterToken = reserved "greater?"

lessExpr :: Parser Expr
lessExpr = lessToken *> (parens (Less <$> (expr <* comma) <*> expr))
  where
    lessToken = reserved "less?"

consExpr :: Parser Expr
consExpr = consToken *> (parens (Cons <$> (expr <* comma) <*> expr))
  where
    consToken = reserved "cons"

carExpr :: Parser Expr
carExpr = reserved "car" *> (parens (Car <$> expr))

cdrExpr :: Parser Expr
cdrExpr = reserved "cdr" *> (parens (Cdr <$> expr))

nullExpr :: Parser Expr
nullExpr = reserved "null?" *> (parens (Null <$> expr))

emptyListExpr :: Parser Expr
emptyListExpr = reserved "emptylist" *> pure EmptyList

varExpr :: Parser Expr
varExpr = Var <$> identifier

-- Helpers

comma :: Parser Char
comma = lexeme (char ',')

number :: Parser Number
number = lexeme (Token.decimal lexer)

identifier :: Parser Id
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

lexer :: TokenParser st
lexer = Token.makeTokenParser letDef

letDef :: LanguageDef st
letDef = emptyDef
  { Token.identStart = oneOf ['a'..'z']
  , Token.identLetter = Token.identStart letDef
  , Token.reservedNames =
      [ "add"
      , "car"
      , "cdr"
      , "cons"
      , "div"
      , "else"
      , "emptylist"
      , "equal?"
      , "greater?"
      , "if"
      , "in"
      , "less?"
      , "let"
      , "minus"
      , "mul"
      , "null?"
      , "then"
      , "zero?"
      ]
  }
