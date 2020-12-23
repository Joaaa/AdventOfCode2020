module Day23.Part1 where

import Data.List
import Data.Maybe
type Cup = Int

puzzleInput :: [Cup]
puzzleInput = map (read . (:[])) "586439172"

playRound :: [Cup] -> [Cup]
playRound cups = selectNext $ [otherCups !! i | i <- [0..destinationIndex]] <> pickedUpCups <> [otherCups !! i | i <- [destinationIndex+1..length otherCups-1]]
    where
        currentCup = head cups
        pickedUpCups = [cups !! i | i <- [1..3]]
        otherCups = [cups !! i | i <- 0:[4..length cups - 1]]
        destinationCup = (maximum [if c < currentCup then c + 10 else c | c <- otherCups]) `mod` 10
        destinationIndex = fromJust $ elemIndex destinationCup otherCups
        selectNext (h:t) = t <> [h]

solution = do
    let solution = (!! 100) $ iterate playRound puzzleInput
    putStrLn $ map (head . show) $ take 8 $ drop 1 $ dropWhile (/=1) $ cycle solution