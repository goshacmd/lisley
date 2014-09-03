import Lisley.Parser
import Lisley.Eval
import Text.Parsec
import System.Environment

run :: String -> String
run input = case (readExpr input) >>= eval of
  Left err  -> "Got an error: " ++ show err
  Right val -> show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn (run (args !! 0))
