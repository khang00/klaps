module LispVal where

import Data.Complex (Complex)

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