module Day13.Part1 where

startTime = 1000299

busses = [41, 37, 971, 17, 13, 23, 29, 487, 19]

earliestTimestamp :: Int -> Int -> Int
earliestTimestamp start bus = (start `div` bus + 1) * bus

solution = print $ (\(a, b) -> (a-startTime) * b) $ minimum $ map (\b -> (earliestTimestamp startTime b, b)) busses