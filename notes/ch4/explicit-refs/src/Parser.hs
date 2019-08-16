module Parser (Parser.parse) where

import Text.Parsec
  (ParseError, (<|>), char, eof, many1, oneOf, option, parse, spaces, try)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Token
  ( LanguageDef, TokenParser
  , identLetter, identStart, reservedNames
  , makeTokenParser
  )

import AST

parse :: String -> Maybe Program
parse input = either (const Nothing) Just (Text.Parsec.parse program "" input)

-- Nonterminals

program :: Parser Program
program = Program <$> (spaces *> expr <* eof)

expr :: Parser Expr
expr
  = (try constExpr <|> diffExpr)
  <|> beginExpr
  <|> zeroExpr
  <|> ifExpr
  <|> letExpr
  <|> letrecExpr
  <|> procExpr
  <|> varExpr
  <|> callExpr
  <|> newrefExpr
  <|> derefExpr
  <|> setrefExpr

constExpr :: Parser Expr
constExpr = Const <$> number

varExpr :: Parser Expr
varExpr = Var <$> identifier

beginExpr :: Parser Expr
beginExpr = Begin <$> ((reserved "begin" *> semiSep1 expr) <* reserved "end")

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

letrecExpr :: Parser Expr
letrecExpr =
  Letrec <$> (letrecToken *> oneOrMoreDecl) <*> (inToken *> expr)
  where
    letrecToken = reserved "letrec"
    decl = Decl <$> identifier <*> (parens identifier) <*> (equal *> expr)
    oneOrMoreDecl = many1 decl

procExpr :: Parser Expr
procExpr = Proc <$> (reserved "proc" *> parens identifier) <*> expr

callExpr :: Parser Expr
callExpr = parens (Call <$> expr <*> expr)

newrefExpr :: Parser Expr
newrefExpr = Newref <$> (reserved "newref" *> (parens expr))

derefExpr :: Parser Expr
derefExpr = Deref <$> (reserved "deref" *> (parens expr))

setrefExpr :: Parser Expr
setrefExpr = reserved "setref" *> (parens (Setref <$> (expr <* comma) <*> expr))

-- Helpers

lexer :: TokenParser st
lexer = makeTokenParser letDef

letDef :: LanguageDef st
letDef = emptyDef
  { identStart = oneOf ['a'..'z']
  , identLetter = identStart letDef
  , reservedNames =
      [ "begin"
      , "deref"
      , "else"
      , "end"
      , "if"
      , "in"
      , "let"
      , "letrec"
      , "newref"
      , "proc"
      , "setref"
      , "then"
      , "zero?"
      ]
  }

inToken :: Parser ()
inToken = reserved "in"

equal :: Parser Char
equal = lexeme (char '=')

number :: Parser Integer
number = lexeme $ (*) <$> sign <*> decimal
  where
    sign = option 1 (char '-' *> pure (-1))

decimal :: Parser Integer
decimal = Token.decimal lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

identifier :: Parser Id
identifier = Token.identifier lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

comma :: Parser String
comma = Token.comma lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Token.semiSep1 lexer
