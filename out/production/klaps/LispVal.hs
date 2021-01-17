module LispVal where

import Control.Monad.Cont (liftIO, liftM)
import Control.Monad.Error.Class (MonadError, catchError)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.Complex (Complex)
import Data.IORef
import GHC.IO.Handle.Types (Handle)
import Text.ParserCombinators.Parsec (ParseError)

instance Show LispVal where show = showVal

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Float
  | Rational Rational
  | Complex (Complex Float)
  | String String
  | Character String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String],
        vararg :: Maybe String,
        body :: [LispVal],
        closure :: Environment
      }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

showVal :: LispVal -> String
showVal (List contents) = "(" ++ wordsList contents ++ ")"
showVal (DottedList x xs) = "(" ++ wordsList x ++ " . " ++ showVal xs ++ ")"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Rational contents) = show contents
showVal (Complex contents) = show contents
showVal (Character contents) = show contents
showVal (Bool True) = "True"
showVal (Bool False) = "False"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = _, closure = _}) =
  "(lambda (" ++ unwords (map show args)
    ++ ( case varargs of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

type ThrowsError = Either LispError

instance Show LispError where show = showError

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

showError :: LispError -> String
showError (UnboundVar message varName) = message ++ ": " ++ varName
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected
    ++ " args; found values "
    ++ wordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError _ = "Error"

wordsList :: [LispVal] -> String
wordsList = unwords . map showVal

type Environment = IORef [(String, IORef LispVal)]

getVar :: Environment -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Environment -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

defineVar :: Environment -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Environment -> [(String, LispVal)] -> IO Environment
bindVars envRef binds = readIORef envRef >>= extendEnv binds >>= newIORef
  where
    extendEnv binds' env = liftM (++ env) (mapM addBind binds')
    addBind (var, value) = do
      ref <- newIORef value
      return (var, ref)

isBound :: Environment -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

nullEnv :: IO Environment
nullEnv = newIORef []
