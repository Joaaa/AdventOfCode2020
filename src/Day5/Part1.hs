module Day5.Part1 where

import Common.FileLoading

parseSeat s = let (r, c) = splitAt 7 s in parseBinary r 0 * 8 + parseBinary c 0

parseBinary "" n = n
parseBinary ('0':t) n = parseBinary t (n*2)
parseBinary ('1':t) n = parseBinary t (n*2+1)

solution = do
    inputs <- readLines (Day 5)
    print $ maximum $ map parseSeat inputs