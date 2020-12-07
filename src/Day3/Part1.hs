module Day3.Part1 where

import Common.FileLoading

getSquares :: [[Char]] -> [Char]
getSquares input = [(input !! y) !! (x `mod` length (head input)) | (y, x) <- zip [0..(length input - 1)] [0,3..]]

solution = do
    input <- readLines (Day 3)
    print $ length $ filter (== '#') $ getSquares input