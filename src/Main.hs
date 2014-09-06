import Lisley.Types
import Lisley.Parser
import Lisley.Eval
import Lisley.Builtins

import Text.Parsec
import Control.Monad
import System.IO
import System.Environment

run :: String -> IO ()
run = evalAndPrint

prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

runAction :: Action String -> IO String
runAction a = runErrorT (trapError a) >>= return . extractValue

evalAndPrint :: String -> IO ()
evalAndPrint e = (runAction . liftM show $ readExpr e >>= eval builtins) >>= putStrLn

repl :: IO ()
repl = prompt "> " >>= evalAndPrint >> repl

main :: IO ()
main = do
  (com:args) <- getArgs

  case com of
    "eval"    -> run $ args !! 0
    "repl"    -> repl
    otherwise -> putStrLn $ "Unknown command: " ++ com
