module Day5.Part2 where

import Common.FileLoading

parseSeat :: [Char] -> Int
parseSeat s = let (r, c) = splitAt 7 s in parseBinary r 0 * 8 + parseBinary c 0

parseBinary "" n = n
parseBinary ('0':t) n = parseBinary t (n*2)
parseBinary ('1':t) n = parseBinary t (n*2+1)

solution = do
    inputs <- readLines (Day 5)
    let seats = map parseSeat inputs
    let min = minimum seats
    let max = maximum seats
    print $ head $ filter (`notElem` seats) [min..max]