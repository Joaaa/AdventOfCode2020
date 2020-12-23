module Day23.Part2 where

import Data.List
import Data.Maybe
import qualified Data.Sequence as S
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Foldable (Foldable(toList))
import Debug.Trace

type Cup = Int
type Cups = S.Seq Int

puzzleInput :: Cups
puzzleInput = S.fromList (map (read . (:[])) "389125467" <> [10..1000000])

playRound :: Cups -> Cups
playRound cups = selectNext $ S.take (destinationIndex+1) otherCups <> pickedUpCups <> S.take (length otherCups - destinationIndex - 1) (S.drop (destinationIndex+1) otherCups)
    where
        currentCup = sHead cups
        pickedUpCups = S.take 3 $ S.drop 1 cups
        otherCups = sHead cups :<| S.drop 4 cups
        destinationCup = head [c | c <- [currentCup-1, currentCup-2, currentCup-3, currentCup-4, 1000000, 999999, 999998, 999997], c > 0, c `notElem` pickedUpCups]
        destinationIndex = head $ catMaybes [case otherCups S.!? i of Just cup -> if cup == destinationCup then Just i else Nothing; _ -> Nothing | i <- interleavedIndices otherCups]
        selectNext (h :<| t) = t :|> h
        sHead (h :<| _) = h

interleavedIndices seq = let l = S.length seq in concat [[a, b] | (a, b) <- zip [0..(l `div` 2)-1] [l-1, l-2 .. (l `div` 2)]]

solution = do
    -- print $ map (S.take 20) $ take 10 $ iterate playRound puzzleInput
    let solution = (!! 1000) $ iterate playRound puzzleInput
    print $ S.take 8 $ S.drop 1 $ S.dropWhileL (/=1) solution