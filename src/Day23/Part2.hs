module Day23.Part2 where

import qualified Data.Sequence as S

type Cup = Int
type Cups = S.Seq Int

puzzleInput :: [Cup]
puzzleInput = map (read . (:[])) "586439172" <> [10..1000000]

createCups :: [Cup] -> Cups
createCups input = foldr (\(i, v) -> S.adjust' (const v) i) (S.replicate (length input + 1) 0) [(a, b) | (a, b) <- zip input (drop 1 input <> [head input])]

(!) :: S.Seq a -> Int -> a
(!) = S.index

playRound :: (Cups, Cup) -> (Cups, Cup)
playRound (cups, currentCup) = (cups', cups' ! currentCup) where
    next = (cups !)
    cupsToMove = take 3 $ drop 1 $ iterate next currentCup
    destinationCup = head [c | c <- [currentCup-1, currentCup-2, currentCup-3, currentCup-4, 1000000, 999999, 999998, 999997], c > 0, c `notElem` cupsToMove]
    cups' = foldr (uncurry S.update) cups [(currentCup, next $ last cupsToMove), (last cupsToMove, next destinationCup), (destinationCup, head cupsToMove)]

solution = do
    let solution = fst $ (!! 10000000) $ iterate playRound (createCups puzzleInput, head puzzleInput)
    print (solution ! 1)
    print (solution ! (solution ! 1))

