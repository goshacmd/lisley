import Lisley.Types
import Lisley.Parser
import Lisley.Eval
import Lisley.Builtins

import Text.Parsec
import Control.Monad
import System.IO
import System.Environment

trapError action = catchError action (return . show)

extractValue :: Action a -> a
extractValue (Right val) = val

run :: String -> String
run input = case readExpr input >>= eval builtins of
  Left err  -> "Got an error: " ++ show err
  Right val -> show val

prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint e = (return . extractValue . trapError . liftM show $ readExpr e >>= eval builtins) >>= putStrLn

repl :: IO ()
repl = prompt "> " >>= evalAndPrint >> repl

main :: IO ()
main = do
  (com:args) <- getArgs

  case com of
    "eval"    -> putStrLn (run (args !! 0))
    "repl"    -> repl
    otherwise -> putStrLn $ "Unknown command: " ++ com
