import Lisley.Parser
import Lisley.Eval
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
