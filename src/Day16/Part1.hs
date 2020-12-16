module Day16.Part1 where

import Text.Parsec
import Common.Utils
import Common.FileLoading
type Range = (Int, Int)

data Rule = Rule String Range Range deriving Show

newtype Ticket = Ticket [Int] deriving Show

data Input = Input {
    rules :: [Rule],
    yourTicket :: Ticket,
    tickets :: [Ticket]
} deriving Show

parseRule :: Parser Rule
parseRule = do
    name <- many $ noneOf ":"
    string ": "
    a <- read <$> many digit
    char '-'
    b <- read <$> many digit
    string " or "
    c <- read <$> many digit
    char '-'
    d <- read <$> many digit
    return $ Rule name (a, b) (c, d)

parseTicket :: Parser Ticket
parseTicket = Ticket <$> sepBy1 (read <$> many digit) (char ',')

parseInput :: Parser Input
parseInput = Input <$>
    endBy (try parseRule) endOfLine <*>
    (endOfLine *> string "your ticket:" *> endOfLine *> parseTicket <* endOfLine) <*>
    (endOfLine *> string "nearby tickets:" *> endOfLine *> endBy1 parseTicket endOfLine)

rangeMatches (a, b) i = a <= i && b >= i

ruleMatches :: Rule -> Int -> Bool
ruleMatches (Rule _ r1 r2) i = rangeMatches r1 i || rangeMatches r2 i

solution = do
    input <- readParsed (Day 16) parseInput
    let r = sum [if any (`ruleMatches` i) (rules input) then 0 else i | (Ticket ticket) <- tickets input, i <- ticket]
    print r