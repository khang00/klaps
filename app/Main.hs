module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readHex, readOct, readFloat)

main :: IO ()
main = do
          expr <- getLine
          putStrLn (readExpr expr)


readExpr :: String -> String
readExpr input = case parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Character String
             | Bool Bool
             deriving Show

parseExpr :: SourceName -> String -> Either ParseError LispVal
parseExpr = parse $ parseAtom
         <|> parseString
         <|> parseCharacter
         <|> parseFloat
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
parseFloat = do _ <- try $ string "#f"
                many1 (digit <|> char '.') >>= (return . Float. fst . head . readFloat)
                

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