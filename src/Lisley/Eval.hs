module Lisley.Eval where

import Lisley.Types
import Data.List (break, nub)
import Data.Maybe (maybe, isJust)

eval :: Env -> Expr -> Action Expr
eval env n@(Number _)  = return n
eval env k@(Keyword _) = return k
eval env s@(String _)  = return s
eval env b@(Bool _)    = return b
eval env v@(Vector _)  = return v
eval env (List [Symbol "quote", v]) = return v
eval env (List (Symbol "do" : exprs)) = mapM (eval env) exprs >>= return . last
eval env (List (Symbol "fn" : Symbol name : Vector params : body)) = do
  (bindings, variadic) <- argsVector params
  return $ Function name bindings variadic (List $ [Symbol "do"] ++ body) env
eval env (List (Symbol "fn" : Vector params : body)) = do
  (bindings, variadic) <- argsVector params
  return $ Function "noname" bindings variadic (List $ [Symbol "do"] ++ body) env
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
apply env (PrimitiveFunction name f) args = f env args
apply env f@(Function name params vararg body closure) args =
  forceArity (arity f) args >> eval (a ++ env) body
  where a = fnArgs params vararg args
apply env f args = throwError $ NotFunction "Not a function" (show f)

arity :: Expr -> (Int, Bool)
arity (Function name params vararg body closure) = (length params, isJust vararg)

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

symbolName :: Expr -> Action String
symbolName (Symbol a) = return a
symbolName v          = throwError $ BadSpecialForm "Symbols are expected in function parameters vector, got" v

allSymbolsUnique :: [String] -> Action [String]
allSymbolsUnique syms = check allUnique syms $ BadSpecialForm "All function argument bindings should be unique, got"
                                                              (_bindings syms)

argsAndVararg :: [String] -> Action ([String], Maybe String)
argsAndVararg = oneVararg . break (== "&")
  where oneVararg (xs, [])         = return (xs, Nothing)
        oneVararg (xs, ["&", var]) = return (xs, Just var)
        oneVararg (xs, badVar)     =
          throwError $ BadSpecialForm "Invalid variadic binding, expected '&' followed by one symbol, got"
                                      (_bindings $ xs ++ badVar)

allUnique :: Eq a => [a] -> Bool
allUnique xs = length xs == length (nub xs)

isFunction :: Expr -> Bool
isFunction (PrimitiveFunction n f) = True
isFunction (Function n p v b c)    = True
isFunction notFunction             = False

_bindings :: [String] -> Expr
_bindings = Vector . map Symbol

check :: MonadError e m => (a -> Bool) -> a -> e -> m a
check f x e = if f x then return x else throwError e
