module Main where

import Data.Time 

import Day25.Part1

main = do
    t <- getCurrentTime 
    putStrLn "Running main."
    solution
    t' <- getCurrentTime 
    print $ diffUTCTime t' t