module Lisley.Eval where

import Data.Maybe (maybe)

data Expr = Atom String
          | List [Expr]
          | Number Int
          | String String
          | Bool Bool

showExpr :: Expr -> String
showExpr (Atom a)   = a
showExpr (Number n) = show n
showExpr (String s) = show s
showExpr (Bool b)   = show b
showExpr (List xs)  = show xs

instance Show Expr where show = showExpr

eval :: Expr -> Expr
eval n@(Number _)             = n
eval s@(String _)             = s
eval b@(Bool _)               = b
eval (List [Atom "quote", v]) = v
eval (List (Atom fn : args))  = apply fn $ map eval args

apply :: String -> [Expr] -> Expr
apply fn args = maybe (Bool False) ($ args) $ lookup fn builtins

builtins = [("+", binNumberFn (+)),
            ("-", binNumberFn (-))]

binNumberFn :: (Int -> Int -> Int) -> [Expr] -> Expr
binNumberFn f [Number a, Number b] = Number $ f a b
binNumberFn _ _                    = Number 0
