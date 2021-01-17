module Main where
import LispParser
import LispEvaluator
import Control.Monad.Cont (liftM)
import LispError (trapError, extractValue)

main :: IO ()
main = do
     args <- getLine
     evaled <- return $ liftM show $ readExpr args >>= eval
     putStrLn $ extractValue $ trapError evaled
