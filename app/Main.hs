module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readHex, readOct, readFloat)
import Data.Ratio ((%))
import Data.Complex (Complex (..))

main :: IO ()
main = do
          expr <- getLine
          putStrLn (readExpr expr)


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Rational Rational
             | Complex (Complex Float)
             | String String
             | Character String
             | Bool Bool
             deriving Show

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseCharacter
         <|> try parseFloat
         <|> try parseRational
         <|> try parseComplex
         <|> parseNumber

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "True" -> Bool True
                         "False" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseHex <|> parseOct <|> parseDecimalNotation

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= (return . Number . read)

parseDecimalNotation :: Parser LispVal
parseDecimalNotation = do _ <- try $ string "#d"
                          x <- many1 digit
                          (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do _ <- try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hexToDigit x)

hexToDigit :: (Eq a, Num a) => String -> a
hexToDigit x = fst $ head (readHex x)

parseOct :: Parser LispVal
parseOct = do _ <- try $ string "#o"
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
escapedChars = do _ <- char '\\'
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