module Day17.Part1 where

import Common.FileLoading
import qualified Data.Map as M
import Data.Maybe
type Pos = (Int, Int, Int)

type Cube = [Pos]

startState :: [String] -> Cube
startState inputs = [(x, y, 0) | (row, y) <- zip inputs [0..], (c, x) <- zip row [0..], c == '#']

getNeighbors (x, y, z) = [(x+dx, y+dy, z+dz) | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dx /= 0 || dy /= 0 || dz /= 0]

countNeighbors :: Cube -> M.Map Pos Int
countNeighbors [] = M.empty 
countNeighbors (h:t) = foldr (\pos m -> M.insertWith (+) pos 1 m) m (getNeighbors h) where
    m = countNeighbors t

nextCube :: Cube -> M.Map Pos Int -> Cube
nextCube cube nbs = mapMaybe (\(pos, nbs) -> if rule pos nbs then Just pos else Nothing) $ M.toList nbs where
    rule pos 3 = True
    rule pos 2 = pos `elem` cube
    rule pos _ = False

nextIt :: Cube -> Cube
nextIt cube = let nbs = countNeighbors cube in nextCube cube nbs

solution = do
    inputs <- readLines (Day 17)
    let start = startState inputs
    print $ length $ iterate nextIt start !! 6
