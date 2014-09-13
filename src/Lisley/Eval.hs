module Lisley.Eval where

import Lisley.Types
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Applicative ((<$>))

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

macroName :: String -> String
macroName = (++ "___macro")

canExpand :: Env -> String -> IO Bool
canExpand env = isBound env . macroName

expand1 :: Env -> String -> [Expr] -> Action Expr
expand1 env sym exprs = resolveSymbol env (macroName sym) >>= flip (apply env) exprs

expand :: Env -> Expr -> Action Expr
expand env form =
  case form of
    List (Symbol s : exprs) -> do
      isExpandable <- liftIO $ canExpand env s
      if isExpandable
      then expand1 env s exprs >>= expand env
      else List <$> mapM (expand env) (Symbol s : exprs)
    List xs -> List <$> mapM (expand env) xs
    Vector xs -> Vector <$> mapM (expand env) xs
    otherwise -> return form

fullEval :: Env -> Expr -> Action Expr
fullEval env expr = expand env expr >>= eval env

eval :: Env -> Expr -> Action Expr
eval env n@(Number _)  = return n
eval env k@(Keyword _) = return k
eval env s@(String _)  = return s
eval env b@(Bool _)    = return b
eval env Nil           = return Nil
eval env v@(Vector xs) = Vector <$> mapM (eval env) xs
eval env (List [Symbol "quote", v]) = return v
eval env (List (Symbol "defmacro" : Symbol sym : body)) =
  defineSymbol sym (macroName sym)
eval env (List [Symbol "def", Symbol sym, val]) =
  eval env val >>= defineSymbol env sym
eval env (List (Symbol "do" : exprs)) =
  last <$> mapM (eval env) exprs
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
    Nil        -> eval env alt
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
  return . fromMaybe Nil $ Map.lookup arg m
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
