module Main where
import LispParser
import LispEvaluator
import Control.Monad.Cont (liftM)
import LispError (trapError, extractValue)
import System.IO

main :: IO ()
main = do args <- getLine
          case length args of 0 -> runRepl
                              1 -> evalAndPrint $ args
                              _ -> putStrLn "Program takes only 0 or 1 argument"

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do result <- prompt
                                    if predicate result then return ()
                                    else action result >> until_ predicate prompt action

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
