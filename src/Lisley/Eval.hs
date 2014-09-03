module Lisley.Eval where

import Lisley.Types
import Data.Maybe (maybe)

eval :: Env -> Expr -> Action Expr
eval env n@(Number _) = return n
eval env s@(String _) = return s
eval env b@(Bool _)   = return b
eval env v@(Vector _) = return v
eval env (List [Atom "quote", v]) = return v
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    otherwise  -> eval env conseq
eval env (List (Atom fn : args)) = mapM (eval env) args >>= apply fn env
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> Env -> [Expr] -> Action Expr
apply fn env args = maybe (throwError $ UnboundSymbol fn)
                          (($ args) . runFunction)
                    $ lookup fn env

builtins :: [(String, Expr)]
builtins = map (\(n, f) -> (n, Function f))
  [("+", numNumFn (+)),
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
   ("first", first),
   ("rest", rest),
   ("conj", conj),
   ("cons", cons)]

first :: [Expr] -> Action Expr
first [List (x:xs)]   = return x
first [Vector (x:xs)] = return x
first [badArg]        = throwError $ TypeMismatch "list" badArg
first badSingleArg    = throwError $ ArityError 1 badSingleArg

rest :: [Expr] -> Action Expr
rest [List (x:xs)]   = return $ List xs
rest [Vector (x:xs)] = return $ List xs
rest [badArg]        = throwError $ TypeMismatch "list" badArg
rest badSingleArg    = throwError $ ArityError 1 badSingleArg

conj :: [Expr] -> Action Expr
conj [List xs, v]   = return . List $ v:xs
conj [Vector xs, v] = return . Vector $ xs ++ [v]
conj [badArg, v]    = throwError $ TypeMismatch "list" badArg
conj badSingleArg   = throwError $ ArityError 2 badSingleArg

cons :: [Expr] -> Action Expr
cons [v, List xs]   = return $ List (v:xs)
cons [v, Vector xs] = return $ List (v:xs)
cons [badArg, v]    = throwError $ TypeMismatch "list" badArg
cons badSingleArg   = throwError $ ArityError 2 badSingleArg

numNumFn = binFn unpackNumber Number
numBoolFn = binFn unpackNumber Bool
boolBoolFn = binFn unpackBool Bool

binFn :: (Expr -> Action a) -> (b -> Expr) -> (a -> a -> b) -> [Expr] -> Action Expr
binFn unpacker packer fn args = if length args /= 2
                                then throwError $ ArityError 2 args
                                else do left  <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return . packer $ fn left right

unpackNumber :: Expr -> Action Int
unpackNumber (Number n) = return n
unpackNumber v          = throwError $ TypeMismatch "number" v

unpackBool :: Expr -> Action Bool
unpackBool (Bool b) = return b
unpackBool v        = throwError $ TypeMismatch "bool" v

runFunction :: Expr -> [Expr] -> Action Expr
runFunction (Function f) = f
runFunction _ = const (throwError $ NotFunction "Not a function" "<fn>")
