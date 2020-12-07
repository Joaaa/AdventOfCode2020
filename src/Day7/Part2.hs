module Day7.Part2 where

import Common.FileLoading
import Text.Parsec
import Day7.Part1

countBags :: [Rule] -> Bag -> Int
countBags rules bag = case rule of
    Rule _ [] -> 1
    Rule _ bagAmounts -> 1 + sum (map (\(n, b) -> n * countBags rules b) bagAmounts)
    where
    rule = head $ filter (\(Rule b _) -> b == bag) rules

solution = do
    rules <- readParsed (Day 7) (endBy parseRule (char '.' >> endOfLine))
    print $ pred $ countBags rules $ Bag "shiny" "gold"
