module Lisley.Eval where

import Lisley.Types
import Data.List (break, nub)
import Data.Maybe (maybe, isJust)

eval :: Env -> Expr -> Action Expr
eval env n@(Number _) = return n
eval env s@(String _) = return s
eval env b@(Bool _)   = return b
eval env v@(Vector _) = return v
eval env (List [Symbol "fn", Vector params, body]) = do
  (bindings, variadic) <- argsVector params
  return $ Function bindings variadic body env
eval env (List [Symbol "quote", v]) = return v
eval env (List [Symbol "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    otherwise  -> eval env conseq
eval env (List (fn : args)) = do
  f <- eval env fn
  params <- mapM (eval env) args
  apply env f params
eval env (Symbol a) = maybe (throwError $ UnboundSymbol a) return $ lookup a env
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: Env -> Expr -> [Expr] -> Action Expr
apply env (PrimitiveFunction f) args = f args
apply env f@(Function params vararg body closure) args =
  forceArity (arity f) args >> eval (a ++ env) body
  where a = fnArgs params vararg args
apply env f args = throwError $ NotFunction "Not a function" "<fn>"

builtins :: [(String, Expr)]
builtins = map (\(n, f) -> (n, PrimitiveFunction f))
  [("+", numNumBinFn (+)),
   ("-", numNumBinFn (-)),
   ("*", numNumBinFn (*)),
   ("=", numBoolBinFn (==)),
   ("not=", numBoolBinFn (/=)),
   ("<", numBoolBinFn (<)),
   (">", numBoolBinFn (>)),
   ("<=", numBoolBinFn (<=)),
   (">=", numBoolBinFn (>=)),
   ("and", boolBoolBinFn (&&)),
   ("or", boolBoolBinFn (&&)),
   ("not", boolBoolUnFn not),
   ("list?", unFn (return . isList) Bool id),
   ("vector?", unFn (return . isVector) Bool id),
   ("even?", numBoolUnFn even),
   ("odd?", numBoolUnFn odd),
   ("first", first),
   ("rest", rest),
   ("conj", conj),
   ("cons", cons)]

first :: Fn
first [List (x:xs)]   = return x
first [Vector (x:xs)] = return x
first [badArg]        = throwError $ TypeMismatch "list" badArg
first badSingleArg    = throwError $ ArityError 1 False badSingleArg

rest :: Fn
rest [List (x:xs)]   = return $ List xs
rest [Vector (x:xs)] = return $ List xs
rest [badArg]        = throwError $ TypeMismatch "list" badArg
rest badSingleArg    = throwError $ ArityError 1 False badSingleArg

conj :: Fn
conj [List xs, v]   = return . List $ v:xs
conj [Vector xs, v] = return . Vector $ xs ++ [v]
conj [badArg, v]    = throwError $ TypeMismatch "list" badArg
conj badSingleArg   = throwError $ ArityError 2 False badSingleArg

cons :: Fn
cons [v, List xs]   = return $ List (v:xs)
cons [v, Vector xs] = return $ List (v:xs)
cons [v, badArg]    = throwError $ TypeMismatch "list" badArg
cons badSingleArg   = throwError $ ArityError 2 False badSingleArg

numNumBinFn = binFn unpackNumber Number
numBoolBinFn = binFn unpackNumber Bool
numBoolUnFn = unFn unpackNumber Bool
boolBoolBinFn = binFn unpackBool Bool
boolBoolUnFn = unFn unpackBool Bool

binFn :: (Expr -> Action a) -> (b -> Expr) -> (a -> a -> b) -> [Expr] -> Action Expr
binFn unpacker packer fn args = if length args /= 2
                                then throwError $ ArityError 2 False args
                                else do left  <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return . packer $ fn left right

unFn :: (Expr -> Action a) -> (b -> Expr) -> (a -> b) -> [Expr] -> Action Expr
unFn unpacker packer fn [arg] = do
  a <- unpacker arg
  return . packer $ fn a
unFn unpacker packer fn xs    = throwError $ ArityError 1 False xs

arity :: Expr -> (Int, Bool)
arity (Function params vararg body closure) = (length params, isJust vararg)

forceArity :: (Int, Bool) -> [Expr] -> Action ()
forceArity (exp, False) fnd | exp == (length fnd) = return ()
forceArity (exp, True)  fnd | exp <= (length fnd) = return ()
forceArity (exp, var)   fnd = throwError $ ArityError exp var fnd

fnArgs :: [String] -> Maybe String -> [Expr] -> [(String, Expr)]
fnArgs params vararg = (\(p, v) -> zip params p ++ vMap vararg v) . splitAt (length params)
  where vMap (Just v) xs = [(v, List xs)]
        vMap Nothing  xs = []

argsVector :: [Expr] -> Action ([String], Maybe String)
argsVector params = mapM symbolName params >>= allSymbolsUnique >>= argsAndVararg

unpackNumber :: Expr -> Action Int
unpackNumber (Number n) = return n
unpackNumber v          = throwError $ TypeMismatch "number" v

unpackBool :: Expr -> Action Bool
unpackBool (Bool b) = return b
unpackBool v        = throwError $ TypeMismatch "bool" v

symbolName :: Expr -> Action String
symbolName (Symbol a) = return a
symbolName v          = throwError $ BadSpecialForm "Symbols are expected in function parameters vector, got" v

allSymbolsUnique :: [String] -> Action [String]
allSymbolsUnique syms = if allUnique syms
                        then return syms
                        else throwError $ BadSpecialForm "All function argument bindings should be unique, got" (Vector $ map Symbol syms)

argsAndVararg :: [String] -> Action ([String], Maybe String)
argsAndVararg = oneVararg . break (== "&")
  where oneVararg (xs, [])         = return (xs, Nothing)
        oneVararg (xs, ["&", var]) = return (xs, Just var)
        oneVararg (xs, badVar)     =
          throwError $ BadSpecialForm "Invalid variadic binding, expected '&' followed by one symbol, got"
                                      (Vector $ map Symbol xs ++ map Symbol badVar)

allUnique :: Eq a => [a] -> Bool
allUnique xs = length xs == length (nub xs)
