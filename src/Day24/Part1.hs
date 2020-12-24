module Day24.Part1 where

import Text.Parsec
import Data.Functor
import Common.Utils
import Control.Monad
import Common.FileLoading
import qualified Data.Map as M

type Position = (Int, Int)

add (x, y) (x', y') = (x+x', y+y')

data Direction = E | SE | SW | W | NW | NE deriving Show

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

solution = do
    inputs <- readParsed (Day 24) parseInputs
    let result = M.fromListWith (\c _ -> not c) [(foldl (flip applyDirection) (0, 0) line, True) | line <- inputs]
    print $ M.size $ M.filter id result