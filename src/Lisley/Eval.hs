module Lisley.Eval where

import Lisley.Types
import Data.Maybe (maybe)

eval :: Expr -> Expr
eval n@(Number _)             = n
eval s@(String _)             = s
eval b@(Bool _)               = b
eval (List [Atom "quote", v]) = v
eval (List (Atom fn : args))  = apply fn $ map eval args

apply :: String -> [Expr] -> Expr
apply fn args = maybe (Bool False) ($ args) $ lookup fn builtins

builtins = [("+", binNumberFn (+)),
            ("-", binNumberFn (-)),
            ("*", binNumberFn (*))]

binNumberFn :: (Int -> Int -> Int) -> [Expr] -> Expr
binNumberFn f [Number a, Number b] = Number $ f a b
binNumberFn _ _                    = Number 0
