module Day2.Part2 where

import Day2.Part1 (Password(..), readInputs, parseInput)
import Data.Bits (Bits(xor))

isValid (Password min max c pw) = (pw !! (min-1) == c) `xor` (pw !! (max-1) == c)

countValid :: [Password] -> Int
countValid = length . filter isValid

solution = do
    passwords <- map parseInput <$> readInputs
    print $ countValid passwords