import Lisley.Types
import Lisley.Parser
import Lisley.Eval
import Lisley.Builtins

import Text.Parsec
import Control.Monad
import System.IO
import System.Environment

run :: String -> IO ()
run expr = defaultEnv >>= flip evalAndPrint expr

repl :: Env -> IO ()
repl env = prompt "> " >>= evalAndPrint env >> repl env

main :: IO ()
main = do
  (com:args) <- getArgs

  case com of
    "eval"    -> run $ args !! 0
    "repl"    -> defaultEnv >>= repl
    otherwise -> putStrLn $ "Unknown command: " ++ com

prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =
  (runAction . liftM show $ readExpr expr >>= eval env) >>= putStrLn

