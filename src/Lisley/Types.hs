module Lisley.Types
  ( module Lisley.Types
  , module Control.Monad.Error
  ) where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Control.Monad.Error
import Control.Applicative ((<$>))
import Text.ParserCombinators.Parsec (ParseError)

type SimpleFn = [Expr] -> Action Expr
type Fn = Env -> SimpleFn

data Expr = Symbol String
          | List [Expr]
          | Vector [Expr]
          | HashMap (Map Expr Expr)
          | Nil
          | Number Int
          | Keyword String
          | String String
          | Bool Bool
          | PrimitiveFunction String Fn
          | Function { fName :: String, params :: [String], vararg :: Maybe String, body :: Expr, closure :: Env }

data LispError = ArityError Int Bool [Expr]
               | ArgumentError String [Expr]
               | TypeMismatch String Expr
               | NotFunction String String
               | BadSpecialForm String Expr
               | UnboundSymbol String
               | Parser ParseError

type Action = ErrorT LispError IO

type Env = IORef [(String, IORef Expr)]

instance Ord Expr where
  (Symbol a)  `compare` (Symbol b)  = a `compare` b
  (List a)    `compare` (List b)    = a `compare` b
  (Vector a)  `compare` (Vector b)  = a `compare` b
  (HashMap a) `compare` (HashMap b) = a `compare` b
  (Number a)  `compare` (Number b)  = a `compare` b
  (Keyword a) `compare` (Keyword b) = a `compare` b
  (String a)  `compare` (String b)  = a `compare` b
  (Bool a)    `compare` (Bool b)    = a `compare` b
  other1      `compare` other2      = GT

instance Eq Expr where
  (Symbol a)  == (Symbol b)  = a == b
  (List a)    == (List b)    = a == b
  (Vector a)  == (Vector b)  = a == b
  (HashMap a) == (HashMap b) = a == b
  (Number a)  == (Number b)  = a == b
  (Keyword a) == (Keyword b) = a == b
  (String a)  == (String b)  = a == b
  (Bool a)    == (Bool b)    = a == b
  other1      == other2      = False

showExpr :: Expr -> String
showExpr (Symbol a)  = a
showExpr (Number n)  = show n
showExpr (Keyword k) = ":" ++ k
showExpr (String s)  = show s
showExpr (Bool b)    = if b then "true" else "false"
showExpr Nil         = "nil"
showExpr (List xs)   = "(" ++ unwordsCol xs ++ ")"
showExpr (Vector xs) = "[" ++ unwordsCol xs ++ "]"
showExpr (HashMap m) = "{" ++ unwordsCol (Map.foldlWithKey (\acc key v -> acc ++ [key,v]) [] m) ++ "}"
showExpr (PrimitiveFunction name f) =
  "#<primitive-fn:" ++ name ++ ">"
showExpr (Function name params vararg body _) =
  "(fn " ++ (if name == "noname" then "" else name ++ " ") ++ "[" ++ unwords params ++ (if isJust vararg then " & " ++ fromJust vararg else "") ++ "] ...)"

showError :: LispError -> String
showError (ArityError exp var fnd) =
  "Expected " ++ show exp ++ (if var then "+" else "") ++ " args, found " ++ show (length fnd) ++ " args: " ++ "(" ++ unwordsCol fnd ++ ")"
showError (ArgumentError msg exprs) = msg ++ ": " ++ unwordsCol exprs
showError (TypeMismatch exp fnd) = "Invalid type: expected " ++ exp ++ ", found " ++ show fnd
showError (NotFunction msg fn)   = msg ++ ": " ++ show fn
showError (BadSpecialForm msg f) = msg ++ ": " ++ show f
showError (UnboundSymbol sym)    = "Unable to resolve symbol: " ++ sym
showError (Parser parseError)    = "Parse error at " ++ show parseError

instance Show Expr where show = showExpr
instance Show LispError where show = showError
instance Error LispError

runAction :: Action String -> IO String
runAction a = extractValue <$> runErrorT (trapError a)

emptyEnv :: IO Env
emptyEnv = newIORef []

unwordsCol :: [Expr] -> String
unwordsCol = unwords . map showExpr

isList :: Expr -> Bool
isList (List _) = True
isList notList  = False

isVector :: Expr -> Bool
isVector (Vector _) = True
isVector notVector  = False

isHashMap :: Expr -> Bool
isHashMap (HashMap _) = True
isHashMap notHashmap  = False

isCol :: Expr -> Bool
isCol x = isList x || isVector x || isHashMap x

isFn :: Expr -> Bool
isFn (PrimitiveFunction _ _) = True
isFn (Function _ _ _ _ _)    = True
isFn notFunction             = False

trapError action = catchError action (return . show)

extractValue :: Either a b -> b
extractValue (Right val) = val
