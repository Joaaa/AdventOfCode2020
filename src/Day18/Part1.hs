module Day18.Part1 where

import Text.Parsec
import Common.Utils
import Data.Functor
import Common.FileLoading
import Control.Monad
import Control.Applicative (liftA2)

data Operation = Sum | Prod deriving Show

apply Sum = (+)
apply Prod = (*)

data Equation = Number Int | Equation Equation [(Operation, Equation)] deriving Show

parseNumber :: Parser Equation
parseNumber = Number . read <$> many digit

parseParens :: Parser Equation
parseParens = char '(' *> parseEquation <* char ')'

parseTerm = parseParens <|> parseNumber

parseOp :: Parser (Operation, Equation)
parseOp = liftA2 (,) (choice [try (string " + ") $> Sum, string " * " $> Prod]) parseTerm

parseEquation :: Parser Equation
parseEquation = liftA2 Equation parseTerm $ many parseOp

getValue :: Equation -> Int
getValue (Number n) = n
getValue (Equation e []) = getValue e
getValue (Equation e1 ((op, e2):eqs)) = getValue (Equation (Number $ apply op (getValue e1) (getValue e2)) eqs)

solution = do
    equations <- readParsed (Day 18) $ sepBy parseEquation endOfLine
    print $ sum $ map getValue equations