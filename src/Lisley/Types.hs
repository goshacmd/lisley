module Lisley.Types
  ( module Lisley.Types
  , module Control.Monad.Error
  ) where

import Data.Maybe (isJust, fromJust)
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

type SimpleFn = [Expr] -> Action Expr
type Fn = Env -> SimpleFn

data Expr = Symbol String
          | List [Expr]
          | Vector [Expr]
          | Number Int
          | Keyword String
          | String String
          | Bool Bool
          | PrimitiveFunction Fn
          | Function { params :: [String], vararg :: Maybe String, body :: Expr, closure :: Env }

data LispError = ArityError Int Bool [Expr]
               | TypeMismatch String Expr
               | NotFunction String String
               | BadSpecialForm String Expr
               | UnboundSymbol String
               | Parser ParseError

type Action = Either LispError

type Env = [(String, Expr)]

showExpr :: Expr -> String
showExpr (Symbol a)            = a
showExpr (Number n)            = show n
showExpr (Keyword k)           = ":" ++ k
showExpr (String s)            = show s
showExpr (Bool b)              = if b then "true" else "false"
showExpr (List xs)             = "(" ++ unwordsCol xs ++ ")"
showExpr (Vector xs)           = "[" ++ unwordsCol xs ++ "]"
showExpr (PrimitiveFunction _) = "<primitive fn>"
showExpr (Function params vararg body _) =
  "(fn [" ++ unwords params ++ (if isJust vararg then " & " ++ fromJust vararg else "") ++ "] ...)"

showError :: LispError -> String
showError (ArityError exp var fnd) =
  "Expected " ++ show exp ++ (if var then "+" else "") ++ " args, found " ++ show (length fnd) ++ " args: " ++ "(" ++ unwordsCol fnd ++ ")"
showError (TypeMismatch exp fnd) = "Invalid type: expected " ++ exp ++ ", found " ++ show fnd
showError (NotFunction msg fn)   = msg ++ ": " ++ show fn
showError (BadSpecialForm msg f) = msg ++ ": " ++ show f
showError (UnboundSymbol sym)    = "Unable to resolve symbol: " ++ sym
showError (Parser parseError)    = "Parse error at " ++ show parseError

instance Show Expr where show = showExpr
instance Show LispError where show = showError
instance Error LispError

unwordsCol :: [Expr] -> String
unwordsCol = unwords . map showExpr

isList :: Expr -> Bool
isList (List _) = True
isList _        = False

isVector :: Expr -> Bool
isVector (Vector _) = True
isVector _          = False

trapError action = catchError action (return . show)

extractValue :: Action a -> a
extractValue (Right val) = val
