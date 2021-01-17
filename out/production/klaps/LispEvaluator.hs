module LispEvaluator where
import Control.Monad.Error.Class (throwError)
import LispVal
import LispError(ThrowsError, LispError(..))

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "quasiquote", val]) = eval val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval (List []) = return $ LispVal.List []
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args) (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("number?", unaryOp isNumber),
              ("number?", unaryOp isString),
              ("number?", unaryOp isSymbol),
              ("number?", unaryOp isBool),
              ("number?", unaryOp isList),
              ("symbol", unaryOp symbol2string),
              ("string", unaryOp string2symbol)]

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s)   = String s
symbol2string _          = String ""
string2symbol (String s) = Atom s
string2symbol _          = Atom ""

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ []  = throwError $ NumArgs 2 []
unaryOp f (x:_) = return $ f x

isNumber, isString, isSymbol, isBool, isList :: LispVal -> LispVal
isNumber (LispVal.Number _) = LispVal.Bool True
isNumber _ = LispVal.Bool False

isString (LispVal.String _) = LispVal.Bool True
isString _ = LispVal.Bool False

isSymbol (LispVal.Atom _) = LispVal.Bool True
isSymbol _ = LispVal.Bool False

isBool (LispVal.Bool _) = LispVal.Bool True
isBool _ = LispVal.Bool False

isList (LispVal.List _) = LispVal.Bool True
isList _ = LispVal.Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ []  = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in 
                           if null parsed 
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum