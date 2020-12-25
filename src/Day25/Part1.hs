module Day25.Part1 where

modpow :: Int -> Int -> Int -> Int
modpow x 0 _ = 1
modpow x 1 _ = x
modpow x n m | even n = modpow ((x * x) `mod` m) (n `div` 2) m
modpow x n m | odd n = (x * modpow ((x * x) `mod` m) ((n-1) `div` 2) m) `mod` m

solution = do
    print $ take 2 $ filter ((`elem` [14222596, 4057428]) . fst) [(modpow 7 n 20201227, n) | n <- [0 .. 20201226]]
    -- [(4057428,229504),(14222596,15585756)]
    print $ modpow 14222596 2918888 20201227
    print $ modpow 4057428 3616052 20201227