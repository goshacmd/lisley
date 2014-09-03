module Lisley.Eval where

import Lisley.Types
import Data.Maybe (maybe)

eval :: Expr -> ThrowsError Expr
eval n@(Number _)             = return n
eval s@(String _)             = return s
eval b@(Bool _)               = return b
eval (List [Atom "quote", v]) = return v
eval (List (Atom fn : args))  = mapM eval args >>= apply fn
eval badForm                  = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [Expr] -> ThrowsError Expr
apply fn args = maybe (throwError $ NotFunction "Unrecognized primitive function" fn)
                      ($ args)
                $ lookup fn builtins

builtins = [("+", binNumberFn (+)),
            ("-", binNumberFn (-)),
            ("*", binNumberFn (*))]

binNumberFn :: (Int -> Int -> Int) -> [Expr] -> ThrowsError Expr
binNumberFn _ []                   = throwError $ NumArgs 2 []
binNumberFn _ v@[_]                = throwError $ NumArgs 2 v
binNumberFn f [Number a, Number b] = return . Number $ f a b
binNumberFn _ [Number a, b]        = throwError $ TypeMismatch "number" b
binNumberFn _ [a, Number b]        = throwError $ TypeMismatch "number" a
binNumberFn _ v                    = throwError $ NumArgs 2 v
