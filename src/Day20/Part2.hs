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
import qualified Data.Vector as V

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

seaMonster :: Orientation -> [Position]
seaMonster dir = [transformCoord dir (x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1], image !! y !! x == '#'] where
    image = [
            "                  # ",
            "#    ##    ##    ###",
            " #  #  #  #  #  #   "
        ]
    w = length $ head image
    h = length image
    transformCoord (d, FLIPPED) (x, y) = transformCoord (d, UNFLIPPED) (w-x-1, y)
    transformCoord (UP, _) (x, y) = (x, y)
    transformCoord (RIGHT, _) (x, y) = (h-y-1, x)
    transformCoord (DOWN, _) (x, y) = (w-x-1, h-y-1)
    transformCoord (LEFT, _) (x, y) = (y, w-x-1)

data Dir = UP | RIGHT | DOWN | LEFT deriving (Show, Eq, Enum)
data Flip = UNFLIPPED | FLIPPED deriving (Show, Eq, Enum)
type Orientation = (Dir, Flip)
type PlacedTile = (TileWithBorders, Orientation)

type BorderMap = M.Map Int [TileWithBorders]

oppD UP = DOWN
oppD RIGHT = LEFT
oppD DOWN = UP
oppD LEFT = RIGHT

oppF UNFLIPPED = FLIPPED
oppF FLIPPED = UNFLIPPED

indexD d = 2 * fromEnum d
indexF f = fromEnum f

flip' :: Orientation -> Orientation
flip' (d, f) = (if d `elem` [UP, DOWN] then d else oppD d, oppF f)

rotate (d, f) d' = (toEnum ((fromEnum d + fromEnum d') `mod` 4), f)

applyOr :: Orientation -> Orientation -> Orientation
applyOr dir (d, f) = rotate (if f == FLIPPED then flip' dir else dir) d

getBorder dir or bs = let (d, f) = applyOr dir or in bs !! (indexD d + indexF f)

allOrientations :: [Orientation]
allOrientations = [(d, f) | d <- [UP ..], f <- [UNFLIPPED ..]]

tilesInDirection :: BorderMap -> PlacedTile -> Dir -> [PlacedTile]
tilesInDirection borderMap tile@(TWB (Tile id _) bs, or) dir = if length (borderMap M.! border) > 1 then tile : tilesInDirection borderMap (ttr, ttrOr) dir else [tile] where
    border = getBorder (dir, UNFLIPPED) or bs
    ttr@(TWB _ rbs) = head $ filter (\(TWB (Tile id' _) _) -> id' /= id) $ borderMap M.! border
    ttrOr = head [ttrOr | ttrOr <- allOrientations, getBorder (oppD dir, FLIPPED) ttrOr rbs == border]

gridValueAt :: Position -> Tile -> Bool 
gridValueAt (x, y) (Tile _ grid) | x >= 0 && y >= 0 && x < 8 && y < 8 = grid !! (y+1) !! (x+1)

transformCoord :: Orientation -> Position -> Position
transformCoord (d, FLIPPED) (x, y) = transformCoord (d, UNFLIPPED) (7-x, y)
transformCoord (UP, _) (x, y) = (x, y)
transformCoord (RIGHT, _) (x, y) = (7-y, x)
transformCoord (DOWN, _) (x, y) = (7-x, 7-y)
transformCoord (LEFT, _) (x, y) = (y, 7-x)

imageValueAt :: Position -> [[PlacedTile]] -> Bool 
imageValueAt (x, y) tiles = gridValueAt (transformCoord or (x', y')) tile where
    tx = x `div` 8
    ty = y `div` 8
    x' = x `mod` 8
    y' = y `mod` 8
    (TWB tile _, or) = tiles !! ty !! tx

possibleOffsets :: [Position] -> [Position]
possibleOffsets positions = [(x, y) | x <- [negate minX .. 12 * 8 - maxX - 1], y <- [negate minY .. 12 * 8 - maxY - 1]] where
    minX = minimum $ map fst positions
    minY = minimum $ map snd positions
    maxX = maximum $ map fst positions
    maxY = maximum $ map snd positions

match :: V.Vector Bool -> Position -> Bool
match image (x, y) = image V.! (x + 12*8*y)

matches :: V.Vector Bool -> Position -> [Position] -> Bool
matches image offset = all (match image . add offset)

add (x, y) (x', y') = (x+x', y+y')

solution = do
    tiles <- map addBorders <$> readParsed (Day 20) (many parseTile)
    let allBorders = concatMap (\(TWB _ b) -> b) tiles
    let countBorders b = length $ filter (==b) allBorders
    let topLeft@(TWB _ tlbs) = head $ filter (\(TWB t b) -> 4 == length (filter (\b -> countBorders b >= 2) b)) tiles
    let tlOr = head [(i, UNFLIPPED) | i <- [UP ..], countBorders (getBorder (UP, UNFLIPPED) (i, UNFLIPPED) tlbs) == 1, countBorders (getBorder (LEFT, UNFLIPPED) (i, UNFLIPPED) tlbs) == 1]
    let borderMap = M.fromListWith (<>) $ concatMap (\t@(TWB _ bs) -> map (,[t]) bs) tiles
    let leftTiles = tilesInDirection borderMap (topLeft, tlOr) DOWN
    let tileGrid = map (\t -> tilesInDirection borderMap t RIGHT) leftTiles
    let image = V.fromList [imageValueAt (i `mod` (12*8), i `div` (12*8)) tileGrid | i <- [0 .. 12 * 12 * 8 * 8 - 1]]
    let foundSeamonsters = [[(offs, map (add offs) (seaMonster or)) | offs <- possibleOffsets (seaMonster or), matches image offs (seaMonster or)] | or <- allOrientations]
    print foundSeamonsters
    let allPosses = concatMap snd $ foundSeamonsters !! 7
    unless (null [p | p <- allPosses, length (filter (==p) allPosses) /= 1]) $ putStrLn "Duplicates found"
    let nbMonsters = maximum $ map length foundSeamonsters
    print nbMonsters
    print $ length (filter id $ V.toList image) - length allPosses