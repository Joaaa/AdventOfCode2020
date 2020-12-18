module Day18.Part1 where

import Text.Parsec
import Common.Utils
import Data.Functor
import Common.FileLoading
import Control.Monad

newtype Operation = Operation (Int -> Int -> Int)
instance Show Operation where
    show _ = "(?)"

data Equation = Number Int | Brackets Equation | Op Operation Equation Equation deriving Show

parseOperation :: Parser Operation
parseOperation = Operation <$> choice [char '+' $> (+), char '*' $> (*)]

parseNumber :: Parser Equation
parseNumber = Number . read <$> many digit

parseParens :: Parser Equation
parseParens = Brackets <$> (char '(' *> parseEquation <* char ')')

parseOp :: Parser Equation
parseOp = do
    l <- try $ (parseParens <|> parseNumber) <* char ' '
    operation <- parseOperation
    char ' '
    Op operation l <$> parseEquation

parseEquation :: Parser Equation
parseEquation = parseOp <|> parseParens <|> parseNumber

getValue :: Equation -> Int
getValue (Number n) = n
getValue (Brackets e) = getValue e
getValue (Op (Operation operation) l (Number r)) = operation (getValue l) r
getValue (Op (Operation operation) l (Op operation' l1 r)) = getValue $ Op operation' (Number $ operation (getValue l) (getValue l1)) r
getValue (Op (Operation operation) l r) = operation (getValue l) (getValue r)

solution = do
    equations <- readParsed (Day 18) $ sepBy parseEquation endOfLine
    print $ sum $ map getValue equations