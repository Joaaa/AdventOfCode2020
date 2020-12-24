module Day24.Part2 where

import Text.Parsec
import Data.Functor
import Common.Utils
import Control.Monad
import Common.FileLoading
import qualified Data.Map as M
import Data.Maybe

type Position = (Int, Int)

add (x, y) (x', y') = (x+x', y+y')

data Direction = E | SE | SW | W | NW | NE deriving (Eq, Enum, Show)

parseDirection :: Parser Direction
parseDirection = choice [char 'e' $> E, try $ string "se" $> SE, try $ string "sw" $> SW, string "w" $> W, try $ string "nw" $> NW, try $ string "ne" $> NE]

parseInputs :: Parser [[Direction]]
parseInputs = sepBy (many1 parseDirection) endOfLine

getOffset :: Direction -> Position
getOffset E = (1, 0)
getOffset SE = (1, -1)
getOffset SW = (0, -1)
getOffset W = (-1, 0)
getOffset NW = (-1, 1)
getOffset NE = (0, 1)

applyDirection :: Direction -> Position -> Position
applyDirection dir = add (getOffset dir)

type Tiles = M.Map Position Bool

directionsToTiles :: Foldable t => [t Direction] -> M.Map Position Bool
directionsToTiles dirs = M.fromListWith (\c _ -> not c) [(foldl (flip applyDirection) (0, 0) line, True) | line <- dirs]

countBlack :: M.Map k Bool -> Int
countBlack = M.size . M.filter id

neigborPositions :: Position -> [Position]
neigborPositions pos = map (add pos . getOffset) [E .. ]

getNeighbors :: Tiles -> M.Map Position Int
getNeighbors = M.foldlWithKey (\m pos v -> if v then foldl (\m k -> M.insertWith (+) k 1 m) m (neigborPositions pos) else m) M.empty

applyRule :: Bool -> Int -> Bool
applyRule True n | n == 0 || n > 2 = False
applyRule False 2 = True
applyRule b _ = b

nextDay :: Tiles -> Tiles
nextDay tiles = M.foldlWithKey (\m tile numNeighbors -> if applyRule (Just True == (tiles M.!? tile)) numNeighbors then M.insert tile True m else m) M.empty $ getNeighbors tiles

solution = do
    inputs <- readParsed (Day 24) parseInputs
    let tiles = directionsToTiles inputs
    print $ countBlack $ (!! 100) $ iterate nextDay tiles