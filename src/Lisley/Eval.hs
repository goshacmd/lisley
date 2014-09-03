module Lisley.Eval where

import Lisley.Types
import Data.Maybe (maybe)

eval :: Expr -> ThrowsError Expr
eval n@(Number _) = return n
eval s@(String _) = return s
eval b@(Bool _)   = return b
eval (List [Atom "quote", v]) = return v
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    otherwise  -> eval conseq

eval (List (Atom fn : args)) = mapM eval args >>= apply fn
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [Expr] -> ThrowsError Expr
apply fn args = maybe (throwError $ NotFunction "Unrecognized primitive function" fn)
                      ($ args)
                $ lookup fn builtins

builtins = [("+", numNumFn (+)),
            ("-", numNumFn (-)),
            ("*", numNumFn (*)),
            ("=", numBoolFn (==)),
            ("not=", numBoolFn (/=)),
            ("<", numBoolFn (<)),
            (">", numBoolFn (>)),
            ("<=", numBoolFn (<=)),
            (">=", numBoolFn (>=)),
            ("and", boolBoolFn (&&)),
            ("or", boolBoolFn (&&)),
            ("head", fnHead),
            ("tail", fnTail),
            ("conj", fnConj)]

fnHead :: [Expr] -> ThrowsError Expr
fnHead [List (x:xs)] = return x
fnHead [badArg]      = throwError $ TypeMismatch "list" badArg
fnHead badSingleArg  = throwError $ NumArgs 1 badSingleArg

fnTail :: [Expr] -> ThrowsError Expr
fnTail [List (x:xs)] = return $ List xs
fnTail [badArg]      = throwError $ TypeMismatch "list" badArg
fnTail badSingleArg  = throwError $ NumArgs 1 badSingleArg

fnConj :: [Expr] -> ThrowsError Expr
fnConj [List xs, v] = return $ List (v:xs)
fnConj [badArg, v]  = throwError $ TypeMismatch "list" badArg
fnConj badSingleArg = throwError $ NumArgs 2 badSingleArg

numNumFn = binFn unpackNumber Number
numBoolFn = binFn unpackNumber Bool
boolBoolFn = binFn unpackBool Bool

binFn :: (Expr -> ThrowsError a) -> (b -> Expr) -> (a -> a -> b) -> [Expr] -> ThrowsError Expr
binFn unpacker packer fn args = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left  <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return . packer $ fn left right

unpackNumber :: Expr -> ThrowsError Int
unpackNumber (Number n) = return n
unpackNumber v          = throwError $ TypeMismatch "number" v

unpackBool :: Expr -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool v        = throwError $ TypeMismatch "bool" v
