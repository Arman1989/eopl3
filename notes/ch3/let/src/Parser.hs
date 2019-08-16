module Parser (Parser.parse) where

import Text.Parsec
  (ParseError, (<|>), char, eof, oneOf, option, parse, spaces, try)
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
  <|> zeroExpr
  <|> ifExpr
  <|> letExpr
  <|> varExpr

constExpr :: Parser Expr
constExpr = Const <$> number

varExpr :: Parser Expr
varExpr = Var <$> identifier

diffExpr :: Parser Expr
diffExpr = minus *> (parens (Diff <$> (expr <* comma) <*> expr))
  where
    minus = char '-'
    comma = lexeme (char ',')

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

-- Helpers

lexer :: TokenParser st
lexer = makeTokenParser letDef

letDef :: LanguageDef st
letDef = emptyDef
  { identStart = oneOf ['a'..'z']
  , identLetter = identStart letDef
  , reservedNames = ["else", "if", "in", "let", "then", "zero?"]
  }

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
