module Main where
import LispParser

main :: IO ()
main = do
          expr <- getLine
          putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input = case LispParser.parseLisp "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> show val
