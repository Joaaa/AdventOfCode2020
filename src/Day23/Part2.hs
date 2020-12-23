module Day23.Part2 where

import Data.List
import Data.Maybe
import Data.Foldable (Foldable(toList))
import qualified Data.Map.Strict as M
import Debug.Trace

type Cup = Int
type Cups = [M.Map Cup Int]

puzzleInput :: Cups
puzzleInput = toCups (map (read . (:[])) "389125467" <> [10..1000000])

toCups :: [Cup] -> Cups
toCups l = map (M.fromList . flip zip [0..]) $ takeWhile (not . null) $ map (take 1000) $ iterate (drop 1000) l

-- Get cup at index i
getCup :: Cups -> Int -> Cup
getCup cups i | i > 1000000 = getCup cups (i `mod` 1000000)
getCup (h:t) i | i >= M.size h = getCup t (i - M.size h)
getCup (h:_) i = head $ mapMaybe (\(k, v) -> if v == i then Just k else Nothing) $ M.toList h

-- Get cup index by value
getCupIndex :: Cups -> Cup -> Int
getCupIndex cups cup = head $ mapMaybe (M.!? cup) cups

-- Remove cup by value
removeCup :: Cups -> Cup -> Cups
removeCup cups cup = [if cup `M.member` m then updateMap m else m | m <- cups]
    where
        updateMap m = let v = m M.! cup in M.map (\i -> if i > v then i-1 else i) (M.delete cup m)

-- Add cup after destionation cup by value
addCup :: Cups -> Cup -> Cup -> Cups
addCup cups destination cup = [if destination `M.member` m then updateMap m else m | m <- cups]
    where
        updateMap m = let v = m M.! destination in M.insert cup (v+1) $ M.map (\i -> if i > v then i+1 else i) m
        

playRound :: (Cups, Int) -> (Cups, Int)
playRound (cups, i) = (nextCups, (if destinationIndex < i then i+4 else i+1) `mod` 1000000)
    where
        currentCup = getCup cups i
        pickedUpCups = [getCup cups j | j <- [i+1 .. i+3]]
        destinationCup = head [c | c <- [currentCup-1, currentCup-2, currentCup-3, currentCup-4, 1000000, 999999, 999998, 999997], c > 0, c `notElem` pickedUpCups]
        destinationIndex = getCupIndex cups destinationCup
        cupsRemoved = foldl removeCup cups pickedUpCups
        nextCups = foldl (`addCup` destinationCup) cupsRemoved (reverse pickedUpCups)

solution = do
    let solution = fst $ (!! 10000) $ iterate playRound (puzzleInput, 0)
    print $ head solution