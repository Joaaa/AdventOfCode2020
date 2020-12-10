{-# LANGUAGE ScopedTypeVariables #-}
module Day10.Part2 where

import Common.FileLoading
import Data.List

count :: Int -> Int
count 1 = 1
count 2 = 2
count 3 = 4
count n = count (n-1) + count (n-2) + count (n-3)

solution = do
    (inputs :: [Int]) <- map read <$> readLines (Day 10)
    let device = maximum inputs + 3
    let s = 0 : sort inputs <> [device]
    let diffs = [s !! (i+1) - s !! i | i <- [0 .. length s - 2]]
    let t = map (length . takeWhile (==1)) $ takeWhile (not . null) $ iterate (dropWhile (==3) . dropWhile (==1)) diffs
    print $ product $ map count t