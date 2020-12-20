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
import Debug.Trace

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

data Dir = UP | RIGHT | DOWN | LEFT deriving (Show, Eq, Enum)
data Flip = UNFLIPPED | FLIPPED deriving (Show, Eq, Enum)
type Orientation = (Dir, Flip)
type PlacedTile = (TileWithBorders, Orientation)

type BorderMap = M.Map Int [TileWithBorders]

opposite UP = DOWN
opposite RIGHT = LEFT
opposite DOWN = UP
opposite LEFT = RIGHT

getBorder :: Orientation -> Orientation -> [a] -> a
getBorder (d1, f1) (d2, f2) = (!! (i d1 + i d2 + if f1 == f2 then 0 else 1)) . cycle where
    i UP = 0
    i RIGHT = 2
    i DOWN = 4
    i LEFT = 6

allOrientations :: [Orientation]
allOrientations = [(d, f) | d <- [UP ..], f <- [UNFLIPPED ..]]

tilesInDirection :: BorderMap -> PlacedTile -> Dir -> [PlacedTile]
tilesInDirection borderMap tile@(TWB (Tile id _) bs, or) dir = if length (borderMap M.! border) > 1 then tile : tilesInDirection borderMap (ttr, ttrOr) dir else [tile] where
    border = getBorder (dir, UNFLIPPED) or bs
    ttr@(TWB _ rbs) = head $ filter (\(TWB (Tile id' _) _) -> id' /= id) $ borderMap M.! border
    ttrOr = head [ttrOr | ttrOr <- allOrientations, getBorder (opposite dir, FLIPPED) ttrOr rbs == border]

solution = do
    tiles <- map addBorders <$> readParsed (Day 20) (many parseTile)
    let allBorders = concatMap (\(TWB _ b) -> b) tiles
    let countBorders b = length $ filter (==b) allBorders
    let topLeft@(TWB _ tlbs) = head $ filter (\(TWB t b) -> 4 == length (filter (\b -> countBorders b >= 2) b)) tiles
    let tlOr = head [(i, UNFLIPPED) | i <- [UP ..], countBorders (getBorder (UP, UNFLIPPED) (i, UNFLIPPED) tlbs) == 1, countBorders (getBorder (LEFT, UNFLIPPED) (i, UNFLIPPED) tlbs) == 1]
    let borderMap = M.fromListWith (<>) $ concatMap (\t@(TWB _ bs) -> map (,[t]) bs) tiles
    let leftTiles = tilesInDirection borderMap (topLeft, tlOr) DOWN
    let tileGrid = map (\t -> tilesInDirection borderMap t RIGHT) leftTiles
    print $ length tileGrid
    print $ map length tileGrid
