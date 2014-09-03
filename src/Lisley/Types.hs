module Lisley.Types
  ( module Lisley.Types
  , module Control.Monad.Error
  ) where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

data Expr = Atom String
          | List [Expr]
          | Vector [Expr]
          | Number Int
          | String String
          | Bool Bool
          | Function ([Expr] -> Action Expr)

data LispError = ArityError Integer [Expr]
               | TypeMismatch String Expr
               | NotFunction String String
               | BadSpecialForm String Expr
               | UnboundSymbol String
               | Parser ParseError

type Action = Either LispError

type Env = [(String, Expr)]

showExpr :: Expr -> String
showExpr (Atom a)     = a
showExpr (Number n)   = show n
showExpr (String s)   = show s
showExpr (Bool b)     = if b then "true" else "false"
showExpr (List xs)    = "(" ++ unwordsCol xs ++ ")"
showExpr (Vector xs)  = "[" ++ unwordsCol xs ++ "]"
showExpr (Function _) = "<fn>"

instance Show Expr where show = showExpr

showError :: LispError -> String
showError (ArityError exp fnd)   = "Expected " ++ show exp ++ " args, found " ++ show (length fnd) ++ " args: " ++ "(" ++ unwordsCol fnd ++ ")"
showError (TypeMismatch exp fnd) = "Invalid type: expected " ++ exp ++ ", found " ++ show fnd
showError (NotFunction msg fn)   = msg ++ ": " ++ show fn
showError (BadSpecialForm msg f) = msg ++ ": " ++ show f
showError (UnboundSymbol sym)    = "Unable to resolve symbol: " ++ sym
showError (Parser parseError)    = "Parse error at " ++ show parseError

instance Show LispError where show = showError
instance Error LispError

unwordsCol :: [Expr] -> String
unwordsCol = unwords . map showExpr
