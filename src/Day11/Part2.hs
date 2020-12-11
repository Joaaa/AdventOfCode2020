module Day11.Part2 where

import Common.FileLoading
import qualified Data.Vector as V
import Common.Utils
import Text.Parsec
import Data.Functor

data Plan = Plan {
    seats :: V.Vector Tile,
    r :: Int,
    c :: Int
}

type Position = (Int, Int)
type Direction = (Int, Int)

data Tile = EmptySeat | FullSeat | Floor deriving (Show, Eq)

parseTile :: Parser Tile
parseTile = char 'L' $> EmptySeat <|> char '.' $> Floor

parseMap :: Parser [[Tile]]
parseMap = endBy (many parseTile) endOfLine

createPlan :: [[Tile]] -> Plan
createPlan tiles = Plan (V.fromList $ concat tiles) (length tiles) (length $ head tiles)

getTile :: Plan -> Position -> Tile
getTile (Plan _ r c) (x, y) | x < 0 || y < 0 || x >= c || y >= r = Floor
getTile (Plan v _ c) (x, y) = v V.! (x+y*c)

directions :: [Position]
directions = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

countDirection :: Plan -> Position -> Direction -> Int
countDirection plan@(Plan _ r c) (x, y) (dx, dy) = let (x', y') = (x+dx, y+dy) in
    if x' < 0 || y' < 0 || x' >= c || y' >= r
        then 0
        else case getTile plan (x', y') of
            FullSeat -> 1
            EmptySeat -> 0
            Floor -> countDirection plan (x', y') (dx, dy)

countSurrounding :: Plan -> Position -> Int
countSurrounding plan pos = sum $ map (countDirection plan pos) directions

getPositions :: Plan -> [Position]
getPositions (Plan _ r c) = [(i `mod` c, i `div` c) | i <- [0..r*c-1]]

applyRule :: Tile -> Int -> Tile
applyRule Floor _ = Floor
applyRule EmptySeat 0 = FullSeat
applyRule FullSeat n | n >= 5 = EmptySeat
applyRule s _ = s

nextGen :: Plan -> Plan
nextGen plan@(Plan _ r c) = Plan (V.fromList [applyRule (getTile plan pos) (countSurrounding plan pos) | pos <- getPositions plan]) r c

countFull :: Plan -> Int
countFull (Plan v _ _)= length $ filter (== FullSeat) $ V.toList v

solution = do
    plan <- createPlan <$> readParsed (Day 11) parseMap
    print $ map countFull $ take 1000 $ iterate nextGen plan
