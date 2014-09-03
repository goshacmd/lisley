module Lisley.Parser where

import Lisley.Types
import Control.Monad
import Text.ParserCombinators.Parsec hiding (string)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

string :: Parser Expr
string = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

atom :: Parser Expr
atom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  let atom = first:rest in return $ case atom of
      "true"  -> Bool True
      "false" -> Bool False
      _       -> Atom atom

number :: Parser Expr
number = liftM (Number . read) $ many1 digit

list :: Parser Expr
list = liftM List $ sepBy expr spaces

quoted :: Parser Expr
quoted = do
  char '\''
  x <- expr
  return $ List [Atom "quote", x]

expr :: Parser Expr
expr = string
    <|> atom
    <|> number
    <|> quoted
    <|> do char '('
           x <- try list
           char ')'
           return x

readExpr :: String -> Action Expr
readExpr input = case parse (spaces >> expr) "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val
