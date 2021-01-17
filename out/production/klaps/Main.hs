module Main where
import LispParser
import LispEvaluator
import LispEnvironment
import Control.Monad.Cont (liftM)
import System.IO

main :: IO ()
main = do args <- getLine
          case length args of 0 -> runRepl
                              1 -> runOne $ args
                              _ -> putStrLn "Program takes only 0 or 1 argument"

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do result <- prompt
                                    if predicate result then return ()
                                    else action result >> until_ predicate prompt action

evalAndPrint :: Environment -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn
 
evalString :: Environment -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine
