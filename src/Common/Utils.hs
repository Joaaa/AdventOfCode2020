module Common.Utils where

import Text.Parsec
split :: (Eq a) => [a] ->  [a] -> [[a]]
split _ [] = []
split sep lst = let (h, t) = splitFirst lst in h : split sep (drop (length sep) t) where
    splitFirst [] = ([], [])
    splitFirst l | matches l = ([], l)
    splitFirst (h:t) = let (l1, l2) = splitFirst t in (h:l1, l2)
    matches l = length l >= length sep && and (zipWith (==) sep l)

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (h:t) | a == h = (b:t)
replace _ _ l = l

type Parser = Parsec String ()

bitsToInt :: [Bool] -> Int
bitsToInt bits = sum [if b then 2 ^ i else 0 | (b, i) <- zip (reverse bits) [0..]]