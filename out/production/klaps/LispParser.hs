module LispParser where

import Control.Monad (liftM)
import Control.Monad.Error.Class (throwError)
import Data.Complex (Complex (..))
import Data.Ratio ((%))
import LispVal
import Numeric (readFloat, readHex, readOct)
import Text.ParserCombinators.Parsec hiding (spaces)

readExpr :: String -> ThrowsError LispVal
readExpr input = case parseLisp "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

parseLisp :: SourceName -> String -> Either ParseError LispVal
parseLisp = parse parseExpr

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseCharacter
    <|> try parseFloat
    <|> try parseRational
    <|> try parseComplex
    <|> parseNumber
    <|> parseQuote
    <|> parseQuasiquote
    <|> do
      _ <- char '('
      x <- try parseList <|> parseDottedList
      _ <- char ')'
      return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  x <- endBy parseExpr spaces
  xs <- char '.' >> spaces >> parseExpr
  return $ DottedList x xs

parseQuote :: Parser LispVal
parseQuote = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "True" -> Bool True
    "False" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseHex <|> parseOct <|> parseDecimalNotation

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= (return . Number . read)

parseDecimalNotation :: Parser LispVal
parseDecimalNotation = do
  _ <- try $ string "#d"
  x <- many1 digit
  (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
  _ <- try $ string "#x"
  x <- many1 hexDigit
  return $ Number (hexToDigit x)

hexToDigit :: (Eq a, Num a) => String -> a
hexToDigit x = fst $ head (readHex x)

parseOct :: Parser LispVal
parseOct = do
  _ <- try $ string "#o"
  x <- many1 octDigit
  return $ Number (octToDigit x)

octToDigit :: (Eq a, Num a) => String -> a
octToDigit x = fst $ head (readOct x)

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many $ noneOf "\"\\" <|> escapedChars
  _ <- char '"'
  return $ String x

parseFloat :: Parser LispVal
parseFloat = do
  whole <- many1 digit
  _ <- char '.'
  frac <- many1 digit
  return . Float . fst . head . readFloat $ whole ++ "." ++ frac

parseRational :: Parser LispVal
parseRational = do
  numerator <- many1 digit
  _ <- char '/'
  denominator <- many1 digit
  return $ Rational (read numerator % read denominator)

parseComplex :: Parser LispVal
parseComplex = do
  real <- many1 digit
  _ <- char '+'
  imaginary <- many1 digit
  _ <- char 'i'
  return $ Complex (read real :+ read imaginary)

parseCharacter :: Parser LispVal
parseCharacter = try (string "\\#") >>= (return . String)

escapedChars :: Parser Char
escapedChars = do
  _ <- char '\\'
  x <- oneOf "\\\"\'nrt"
  return $ case x of
    '\\' -> x
    '"' -> x
    '\'' -> x
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'
    _ -> x

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space
