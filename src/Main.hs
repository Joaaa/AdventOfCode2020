module Main where

import Data.Time 

import Day23.Part2

main = do
    t <- getCurrentTime 
    putStrLn "Running main."
    solution
    t' <- getCurrentTime 
    print $ diffUTCTime t' t