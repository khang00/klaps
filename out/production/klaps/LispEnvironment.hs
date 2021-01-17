module LispEnvironment where

import Control.Monad.Cont (liftM, liftIO)
import LispError (trapError, extractValue, LispError(..), ThrowsError)
import Data.IORef
import LispVal
import Control.Monad.Except (ExceptT, throwError, runExceptT)

type Environment = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO

getVar :: Environment -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Environment -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Environment -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do alreadyDefined <- liftIO $ isBound envRef var
                                if alreadyDefined
                                  then setVar envRef var value >> return value
                                  else liftIO $ do valueRef <- newIORef value
                                                   env <- readIORef envRef
                                                   writeIORef envRef ((var, valueRef) : env)
                                                   return value

bindVars :: Environment -> [(String, LispVal)] -> IO Environment
bindVars envRef binds = readIORef envRef >>= extendEnv binds >>= newIORef
                        where extendEnv binds' env = liftM (++ env) (mapM addBind binds')
                              addBind (var, value) = do ref <- newIORef value
                                                        return (var, ref)

isBound :: Environment -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

nullEnv :: IO Environment
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
