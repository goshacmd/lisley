module Lisley.Builtins where

import Lisley.Types
import Lisley.Eval

fnApply :: Fn
fnApply env (f:args) = apply env f args

fnEval :: Fn
fnEval env [expr]  = eval env expr
fnEval env badArgs = throwError $ ArityError 1 False badArgs

builtins :: [(String, Expr)]
builtins = map (\(n, f) -> (n, PrimitiveFunction f))
  [ ("+",       numNumBinFn (+))
  , ("-",       numNumBinFn (-))
  , ("*",       numNumBinFn (*))
  , ("=",       numBoolBinFn (==))
  , ("not=",    numBoolBinFn (/=))
  , ("<",       numBoolBinFn (<))
  , (">",       numBoolBinFn (>))
  , ("<=",      numBoolBinFn (<=))
  , (">=",      numBoolBinFn (>=))
  , ("string=", strBoolBinFn (==))
  , ("and",     boolBoolBinFn (&&))
  , ("or",      boolBoolBinFn (&&))
  , ("not",     boolBoolUnFn not)
  , ("list?",   unFn (return . isList) Bool id)
  , ("vector?", unFn (return . isVector) Bool id)
  , ("even?",   numBoolUnFn even)
  , ("odd?",    numBoolUnFn odd)
  , ("first",   const first)
  , ("rest",    const rest)
  , ("conj",    const conj)
  , ("cons",    const cons)
  , ("keyword", const keyword)
  , ("name",    const name)
  , ("apply",   fnApply)
  , ("map",     fnMap)
  , ("eval",    fnEval)
  , ("fold",    fnFold)
  ]

first :: SimpleFn
first [List (x:xs)]   = return x
first [Vector (x:xs)] = return x
first [badArg]        = throwError $ TypeMismatch "list or vector" badArg
first badArgs         = throwError $ ArityError 1 False badArgs

rest :: SimpleFn
rest [List (x:xs)]   = return $ List xs
rest [Vector (x:xs)] = return $ List xs
rest [badArg]        = throwError $ TypeMismatch "list or vector" badArg
rest badArgs         = throwError $ ArityError 1 False badArgs

conj :: SimpleFn
conj [List xs, v]   = return . List $ v:xs
conj [Vector xs, v] = return . Vector $ xs ++ [v]
conj [badArg, v]    = throwError $ TypeMismatch "list or vector" badArg
conj badArgs        = throwError $ ArityError 2 False badArgs

cons :: SimpleFn
cons [v, List xs]   = return $ List (v:xs)
cons [v, Vector xs] = return $ List (v:xs)
cons [v, badArg]    = throwError $ TypeMismatch "list or vector" badArg
cons badArgs        = throwError $ ArityError 2 False badArgs

keyword :: SimpleFn
keyword [Symbol x] = return $ Keyword x
keyword [String x] = return $ Keyword x
keyword [badArg]   = throwError $ TypeMismatch "symbol or string" badArg
keyword badArgs    = throwError $ ArityError 1 False badArgs

name :: SimpleFn
name [String x]  = return $ String x
name [Symbol x]  = return $ String x
name [Keyword x] = return $ String x
name [badArg]    = throwError $ TypeMismatch "string, symbol, or keyword" badArg
name badArgs     = throwError $ ArityError 1 False badArgs

fnMap :: Fn
fnMap env [f, List xs]   = mapM (apply env f . return) xs >>= return . List
fnMap env [f, Vector xs] = mapM (apply env f . return) xs >>= return . List
fnMap env [f, badArg]    = throwError $ TypeMismatch "list or vector" badArg
fnMap env badArgs        = throwError $ ArityError 2 False badArgs

fnFold :: Fn
fnFold env [f, v, List xs]   = foldM (\acc curr -> apply env f [acc, curr]) v xs
fnFold env [f, v, Vector xs] = foldM (\acc curr -> apply env f [acc, curr]) v xs
fnFold env [f, v, badArg]    = throwError $ TypeMismatch "list or vector" badArg
fnFold env badArgs           = throwError $ ArityError 3 False badArgs

numNumBinFn = binFn unpackNumber Number
numBoolBinFn = binFn unpackNumber Bool
strBoolBinFn = binFn unpackString Bool
numBoolUnFn = unFn unpackNumber Bool
boolBoolBinFn = binFn unpackBool Bool
boolBoolUnFn = unFn unpackBool Bool

binFn :: (Expr -> Action a) -> (b -> Expr) -> (a -> a -> b) -> Env -> [Expr] -> Action Expr
binFn unpacker packer fn env args = if length args /= 2
                                    then throwError $ ArityError 2 False args
                                    else do left  <- unpacker $ args !! 0
                                            right <- unpacker $ args !! 1
                                            return . packer $ fn left right

unFn :: (Expr -> Action a) -> (b -> Expr) -> (a -> b) -> Env -> [Expr] -> Action Expr
unFn unpacker packer fn env [arg] = do
  a <- unpacker arg
  return . packer $ fn a
unFn unpacker packer fn env args  = throwError $ ArityError 1 False args

unpackNumber :: Expr -> Action Int
unpackNumber (Number n) = return n
unpackNumber v          = throwError $ TypeMismatch "number" v

unpackString :: Expr -> Action String
unpackString (String s) = return s
unpackString v          = throwError $ TypeMismatch "string" v

unpackBool :: Expr -> Action Bool
unpackBool (Bool b) = return b
unpackBool v        = throwError $ TypeMismatch "bool" v
