module Lisley.Builtins where

import Lisley.Types
import Lisley.Eval
import qualified Data.Map as Map
import Data.List.Split (chunksOf)

defaultEnv :: IO Env
defaultEnv = emptyEnv >>= flip bindSymbols builtins

builtins :: [(String, Expr)]
builtins = map (\(n, f) -> (n, PrimitiveFunction n f))
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
  , ("hash-map", const hashMap)
  , ("name",    const name)
  , ("apply",   fnApply)
  , ("map",     fnMap)
  , ("eval",    fnEval)
  , ("fold",    fnFold)
  , ("print",   fnPrint)
  ]

hashMap :: [Expr] -> Action Expr
hashMap kvs
  | even (length kvs) = return . HashMap . Map.fromList . map (\[k, v] -> (k, v)) $ chunksOf 2 kvs
  | otherwise         = throwError $ ArgumentError "hash-map expected an even number of arguments, got" kvs

colToList :: Expr -> Action [Expr]
colToList (List xs)   = return xs
colToList (Vector xs) = return xs
colToList badArg      = throwError $ TypeMismatch "list or vector" badArg

first :: SimpleFn
first [col]   = colToList col >>= return . head
first badArgs = throwError $ ArityError 1 False badArgs

rest :: SimpleFn
rest [col]   = colToList col >>= return . List . tail
rest badArgs = throwError $ ArityError 1 False badArgs

conj :: SimpleFn
conj [List xs, v]   = return . List $ v:xs
conj [Vector xs, v] = return . Vector $ xs ++ [v]
conj [badArg, v]    = throwError $ TypeMismatch "list or vector" badArg
conj badArgs        = throwError $ ArityError 2 False badArgs

cons :: SimpleFn
cons [v, col] = colToList col >>= return . List . (v:)
cons badArgs  = throwError $ ArityError 2 False badArgs

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
fnMap env [f, col] = colToList col >>= mapM (apply env f . return) >>= return . List
fnMap env badArgs  = throwError $ ArityError 2 False badArgs

fnFold :: Fn
fnFold env [f, v, col] = colToList col >>= foldM (\acc curr -> apply env f [acc, curr]) v
fnFold env badArgs     = throwError $ ArityError 3 False badArgs

fnPrint :: Fn
fnPrint env (s:rest) = liftIO (print s) >> return s

fnApply :: Fn
fnApply env (f:args) = apply env f args

fnEval :: Fn
fnEval env [expr]  = fullEval env expr
fnEval env badArgs = throwError $ ArityError 1 False badArgs

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
