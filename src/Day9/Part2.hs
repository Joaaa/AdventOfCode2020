{-# LANGUAGE ScopedTypeVariables #-}
module Day9.Part2 where

import Common.FileLoading
import Control.Monad
import Data.List

setSum :: Int -> [Int] -> Int
setSum n l | length l < n = 0
setSum n l = sum $ take n l

solution = do
    (numbers :: [Int]) <- take 542 . map read <$> readLines (Day 9)
    forM_ [0..20] $ \n ->
        print $ map (\(_, l) -> minimum l + maximum l) $ filter ((== 50047984) . fst) $ map (\l -> (setSum n l, take n l)) $ tails numbers