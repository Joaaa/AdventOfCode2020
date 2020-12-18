module Day18.Part2 where

import Text.Parsec
import Common.Utils
import Data.Functor
import Common.FileLoading
import Control.Monad

data Equation = Number Int | Sum [Equation] | Prod [Equation] deriving Show

parseNumber :: Parser Equation
parseNumber = Number . read <$> many digit

parseParens :: Parser Equation
parseParens = char '(' *> parseMult <* char ')'

parseSum :: Parser Equation
parseSum = Sum <$> sepBy (parseParens <|> parseNumber) (try $ string " + ")

parseMult :: Parser Equation
parseMult = Prod <$> sepBy parseSum (string " * ")

getValue :: Equation -> Int
getValue (Number n) = n
getValue (Sum ns) = sum $ map getValue ns
getValue (Prod ns) = product $ map getValue ns

solution = do
    equations <- readParsed (Day 18) $ sepBy parseMult endOfLine
    print $ sum $ map getValue equations