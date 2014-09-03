import Lisley.Parser
import Lisley.Eval
import Text.Parsec
import System.Environment

parseExpr = parse (spaces >> expr) "lisp"

readExpr :: String -> String
readExpr input = case parseExpr input of
  Left err  -> "Failed to parse: " ++ show err
  Right val -> "Parsed: " ++ show val ++ "\n\n" ++ "Evaluated: " ++ show (eval val)

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
