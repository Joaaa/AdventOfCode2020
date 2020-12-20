{-# LANGUAGE TupleSections #-}
module Day20.Part2 where

import Common.Utils
import Text.Parsec
import Data.Functor
import Common.FileLoading
import Control.Monad
import qualified Data.Map as M
import Data.List
import Data.Maybe

type Grid = [[Bool]]
data Tile = Tile Int Grid

instance Show Tile where
    show (Tile id _) = "Tile" <> show id

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
            b <- [head grid, map last grid, reverse (last grid), reverse (map head grid)]
            [b, reverse b]

type Position = (Int, Int)

seaMonster :: [Position]
seaMonster = [(x, y) | x <- [0 .. length (head image) - 1], y <- [0 .. length image - 1], image !! y !! x == '#'] where
    image = [
            "                  # ",
            "#    ##    ##    ###",
            " #  #  #  #  #  #   "
        ]

type PlacedTile = (TileWithBorders, Int)

type BorderMap = M.Map Int [TileWithBorders]

topBorder = (!! 0)
rightBorder = (!! 2)
bottomBorder = (!! 4)
leftBorder = (!! 6)

withOrientation :: Int -> ([a] -> a) -> [a] -> a
withOrientation i f = f . drop i . cycle

tilesToRightOf :: BorderMap -> PlacedTile -> [PlacedTile]
tilesToRightOf borderMap tile@(TWB (Tile id _) bs, or) = if length (borderMap M.! border) > 1 then tile : tilesToRightOf borderMap (ttr, ttrOr) else [tile] where
    border = withOrientation or rightBorder bs
    ttr@(TWB _ rbs) = head $ filter (\(TWB (Tile id' _) _) -> id' /= id) $ borderMap M.! border
    ttrOr = fromJust $ elemIndex border $ drop 6 $ cycle rbs

solution = do
    tiles <- map addBorders <$> readParsed (Day 20) (many parseTile)
    let allBorders = concatMap (\(TWB _ b) -> b) tiles
    let countBorders b = length $ filter (==b) allBorders
    let topLeft@(TWB _ tlbs) = head $ filter (\(TWB t b) -> 4 == length (filter (\b -> countBorders b >= 2) b)) tiles
    let tlOr = head [i | i <- [0, 2, 4, 6], countBorders (withOrientation i topBorder tlbs) == 1, countBorders (withOrientation i leftBorder tlbs) == 1]
    let borderMap = M.fromListWith (<>) $ concatMap (\t@(TWB _ bs) -> map (,[t]) bs) tiles
    print $ length $ tilesToRightOf borderMap (topLeft, tlOr)
    forM_ (take 10 $ M.toList borderMap) print