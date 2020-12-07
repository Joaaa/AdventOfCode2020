module Day6.Part1 where

import Common.FileLoading

lettersPresent l = filter (`elem` l) ['a'..'z']

solution = do
    inputs <- readLines (Day 6)
    print $ sum $ map (length . lettersPresent) inputs