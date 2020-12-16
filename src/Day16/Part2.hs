module Day16.Part2 where

import Text.Parsec
import Common.Utils
import Common.FileLoading
import Data.List
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

isValid :: [Rule] -> Ticket -> Bool
isValid rules (Ticket ns) = all (\i -> any (`ruleMatches` i) rules) ns

-- orderMatches :: [Rule] -> Ticket -> Bool
-- orderMatches rules (Ticket ns) = all (uncurry ruleMatches) $ zip rules ns

findOrder :: [Rule] -> [Ticket] -> [[Rule]]
findOrder [] _ = [[]]
findOrder rules tickets = concat [map (h:) (findOrder t tickets') | (h, t) <- map spl matching] where
    tickets' = map (\(Ticket (h:t)) -> Ticket t) tickets
    matching = [i | i <- [0 .. length rules - 1], all (\(Ticket t) -> rules !! i `ruleMatches` head t) tickets]
    spl i = (rules !! i, [rules !! j | j <- [0 .. length rules - 1], j /= i])

solution = do
    input <- readParsed (Day 16) parseInput
    let valid = filter (isValid $ rules input) $ tickets input
    -- let r = filter (\rules -> all (orderMatches rules) valid) $ permutations $ rules input
    print $ findOrder (rules input) valid