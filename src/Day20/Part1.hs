module Day20.Part1 where

import Common.Utils
import Text.Parsec
import Data.Functor
import Common.FileLoading
import Control.Monad

type Grid = [[Bool]]
data Tile = Tile Int Grid deriving Show
data TileWithBorders = TWB Tile [Int] deriving Show

parseGrid :: Parser Grid
parseGrid = count 10 (count 10 (choice [char '.' $> False, char '#' $> True]) <* endOfLine)

parseTile :: Parser Tile
parseTile = do
    string "Tile "
    n <- read <$> many1 digit
    char ':'
    endOfLine 
    g <- parseGrid
    endOfLine
    return $ Tile n g
    
addBorders :: Tile -> TileWithBorders
addBorders tile@(Tile id grid) = TWB tile $ map bitsToInt borders
    where
        borders = do
            b <- [head grid, map last grid, last grid, map head grid]
            [b, reverse b]

solution = do
    tiles <- map addBorders <$> readParsed (Day 20) (many parseTile)
    let allBorders = concatMap (\(TWB _ b) -> b) tiles
    let countBorders b = length $ filter (==b) allBorders
    let counts = map (\(TWB t b) -> length (filter (\b -> countBorders b >= 2) b)) tiles
    let corners = filter (\(TWB t b) -> 4 == length (filter (\b -> countBorders b >= 2) b)) tiles
    let cornerIds = map (\(TWB (Tile i _) _) -> i) corners
    print cornerIds
    print $ product cornerIds