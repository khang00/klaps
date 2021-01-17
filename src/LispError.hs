module LispError where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error.Class (catchError, MonadError)
import LispVal

type ThrowsError = Either LispError

instance Show LispError where show = showError

data LispError = NumArgs Integer [LispVal]
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
showError (UnboundVar message varName)  = message ++ ": " ++ varName
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ wordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError _ = "Error"

wordsList :: [LispVal] -> String
wordsList = unwords . map showVal

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
