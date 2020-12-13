module Day13.Part2 where

import Common.FileLoading
import Common.Utils
import Data.Maybe
import Control.Monad
import Data.List

data Bus = Bus {
    id :: Int,
    offset :: Int
} deriving (Show, Eq, Ord)

checkBus :: Int -> Bus -> Bool
checkBus baseTime (Bus i offs) = ((baseTime + offs) `mod` i) == 0

solution = do
    schedule <- split "," . head <$> readLines (Day 13)
    let busses' = reverse $ sort $ mapMaybe (\i -> let b = schedule !! i in if b == "x" then Nothing else Just (Bus (read b) (i `mod` read b))) [0..length schedule - 1]
    let ([Bus maxId maxOffs], busses) = splitAt 1 busses'
    forM_ [maxId - maxOffs, 2*maxId - maxOffs .. 10000000000000] $ \baseTime -> do
        when (all (checkBus baseTime) busses) $ print baseTime
    putStrLn "Done."
