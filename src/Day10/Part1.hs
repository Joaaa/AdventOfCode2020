{-# LANGUAGE ScopedTypeVariables #-}
module Day10.Part1 where

import Common.FileLoading
import Data.List

solution = do
    (inputs :: [Int]) <- map read <$> readLines (Day 10)
    let device = maximum inputs + 3
    let s = 0 : sort inputs <> [device]
    let diffs = [s !! (i+1) - s !! i | i <- [0 .. length s - 2]]
    let ones = length $ filter (==1) diffs
    let threes = length $ filter (==3) diffs
    print $ threes * ones