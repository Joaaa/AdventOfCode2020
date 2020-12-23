module Day23.Part2 where

import Data.List
import Data.Maybe
import qualified Data.Sequence as S
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Foldable (Foldable(toList))
import Debug.Trace

type Cup = Int
type Cups = S.Seq Int

puzzleInput :: [Cup]
puzzleInput = map (read . (:[])) "586439172" <> [10..1000000]

createCups :: [Cup] -> Cups
createCups input = foldr (\(i, v) -> S.adjust' (const v) i) (S.replicate (length input + 1) 0) [(a, b) | (a, b) <- zip input (drop 1 input <> [head input])]

s ! i = fromJust $ s S.!? i

playRound :: (Cups, Cup) -> (Cups, Cup)
playRound (cups, currentCup) = (cups''', cups''' ! currentCup) where
    next = (cups !)
    cupsToMove = take 3 $ drop 1 $ iterate next currentCup
    destinationCup = head [c | c <- [currentCup-1, currentCup-2, currentCup-3, currentCup-4, 1000000, 999999, 999998, 999997], c > 0, c `notElem` cupsToMove]
    cups' = S.update currentCup (next $ last cupsToMove) cups
    cups'' = S.update (last cupsToMove) (next destinationCup) cups'
    cups''' = S.update destinationCup (head cupsToMove) cups''

solution = do
    let solution = fst $ (!! 10000000) $ iterate playRound (createCups puzzleInput, head puzzleInput)
    print (solution ! 1)
    print (solution ! (solution ! 1))

