module Lisley.Eval where

import Lisley.Types
import Data.IORef
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import qualified Data.Map as Map
import Control.Arrow ((&&&))

isBound :: Env -> String -> IO Bool
isBound envRef sym = readIORef envRef >>= return . maybe False (const True) . lookup sym

resolveSymbol :: Env -> String -> Action Expr
resolveSymbol envRef sym = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundSymbol sym)
        (liftIO . readIORef)
        (lookup sym env)

defineSymbol :: Env -> String -> Expr -> Action Expr
defineSymbol envRef sym val = do
  isDefined <- liftIO $ isBound envRef sym

  if isDefined
  then liftIO $ do
    env <- readIORef envRef
    flip writeIORef val . fromJust $ lookup sym env
  else liftIO $ do
    valRef <- newIORef val
    modifyIORef envRef ((sym, valRef):)

  return val

bindSymbols :: Env -> [(String, Expr)] -> IO Env
bindSymbols envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) $ mapM addBinding bindings
        addBinding (sym, val) = do ref <- newIORef val
                                   return (sym, ref)

expands :: [(String, [Expr] -> Action Expr)]
expands = [ ("let",  expandLet)
          , ("defn", expandDefn)
          ]

canExpand :: String -> Bool
canExpand s = elem s $ map fst expands

expand1 :: String -> [Expr] -> Action Expr
expand1 s exprs = ($ exprs) $ fromJust $ lookup s expands

expandLet:: [Expr] -> Action Expr
expandLet (Vector bindings : body)
  | even (length bindings) =
    return . List $ (List $ [Symbol "fn", Vector bs] ++ body) : vs
  | otherwise =
    throwError $ BadSpecialForm "let requires an even number of forms in bindings vector" (Vector bindings)
  where (bs, vs) = (map head &&& map (head . tail)) . chunksOf 2 $ bindings
expandLet badForm =
  throwError $ BadSpecialForm "let requires a vector of vindings" $ List (Symbol "let" : badForm)

expandDefn :: [Expr] -> Action Expr
expandDefn (Symbol sym : bindings : body) =
  return $ List [Symbol "def", Symbol sym, List $ [Symbol "fn", Symbol sym, bindings] ++ body]
expandDefn badForm =
  throwError $ BadSpecialForm "defn requires a function name, a vector of bindings, and the function body" $ List (Symbol "defn" : badForm)

expand :: Expr -> Action Expr
expand l@(List (Symbol s : exprs))
  | canExpand s    = expand1 s exprs >>= expand
expand (List xs)   = mapM expand xs >>= return . List
expand (Vector xs) = mapM expand xs >>= return . Vector
expand x           = return x

fullEval :: Env -> Expr -> Action Expr
fullEval env expr = expand expr >>= eval env

eval :: Env -> Expr -> Action Expr
eval env n@(Number _)  = return n
eval env k@(Keyword _) = return k
eval env s@(String _)  = return s
eval env b@(Bool _)    = return b
eval env v@(Vector xs) = mapM (eval env) xs >>= return . Vector
eval env (List [Symbol "quote", v]) = return v
eval env (List [Symbol "def", Symbol sym, val]) =
  eval env val >>= defineSymbol env sym
eval env (List (Symbol "do" : exprs)) =
  mapM (eval env) exprs >>= return . last
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
eval env (Symbol a) = resolveSymbol env a
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: Env -> Expr -> [Expr] -> Action Expr
apply env (PrimitiveFunction name f) args = f env args
apply env f@(Function name params vararg body closure) args = do
  forceArity (arity f) args
  newEnv <- liftIO $ bindSymbols env a
  eval newEnv body
  where a = fnArgs params vararg args
apply env (HashMap m) [arg] =
  return . fromMaybe (List []) $ Map.lookup arg m
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
