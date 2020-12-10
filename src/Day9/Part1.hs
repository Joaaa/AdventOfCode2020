{-# LANGUAGE ScopedTypeVariables #-}
module Day9.Part1 where

import Common.FileLoading
import Data.List

check :: [Int] -> Bool
check l | length l < 26 = True
check l = or [l !! i + l !! j == l !! 25 | i <- [0..24], j <- [i..24]]

solution = do
    (numbers :: [Int]) <- map read <$> readLines (Day 9)
    print $ map (!!25) $ filter (not . check) $ tails numbers