module Lisley.Types where

data Expr = Atom String
          | List [Expr]
          | Number Int
          | String String
          | Bool Bool

showExpr :: Expr -> String
showExpr (Atom a)   = a
showExpr (Number n) = show n
showExpr (String s) = show s
showExpr (Bool b)   = show b
showExpr (List xs)  = "(" ++ unwords (map showExpr xs) ++ ")"

instance Show Expr where show = showExpr
