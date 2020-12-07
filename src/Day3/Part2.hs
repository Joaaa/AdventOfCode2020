module Day3.Part2 where

import Common.FileLoading

data Slope = Slope Int Int

getSquares :: Slope -> [[Char]] -> [Char]
getSquares (Slope right down) input = [(input !! y) !! (x `mod` length (head input)) | (y, x) <- zip [0,down..(length input - 1)] [0,right..]]

count slope = length . filter (== '#') . getSquares slope

solution = do
    input <- readLines (Day 3)
    print $ count (Slope 1 1) input * count (Slope 3 1) input * count (Slope 5 1) input * count (Slope 7 1) input * count (Slope 1 2) input