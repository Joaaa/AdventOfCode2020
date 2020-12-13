module Day13.Part2 where

-- Credit: https://www.iacr.org/archive/pkc2008/49390038/49390038.pdf

import Common.FileLoading
import Common.Utils
import Data.Maybe
import Data.List

data Bus = Bus Integer Integer deriving (Show, Eq, Ord)

data Equation = Equation {
    offset :: Integer,
    modulus :: Integer
} deriving Show

inverse :: Integer -> Integer -> Integer
inverse mi ni = case find (\i -> (mi*i) `mod` ni == 1) [1..(ni-1)] of
    Just r -> r
    Nothing -> error "Inverse not found"

solution = do
    schedule <- split "," . head <$> readLines (Day 13)
    let busses = mapMaybe (\i -> let b = schedule !! i in if b == "x" then Nothing else Just (Bus (read b) (fromIntegral i))) [0..length schedule - 1]
    let eqs = [Equation ((i - (o `mod` i)) `mod` i) i | Bus i o <- busses]
    print eqs
    let ns = map modulus eqs
    print $ "ns: " <> show ns
    let os = map offset eqs
    print $ "os: " <> show os
    let m = product ns
    print $ "m: " <> show m
    let mis = [m `div` n | n <- ns]
    print $ "mis: " <> show mis
    let invs = [inverse mi ni | (mi, ni) <- zip mis ns]
    print $ "invs: " <> show invs
    print [(mi * inv) `mod` n | (n, mi, inv) <- zip3 ns mis invs]
    let t = sum [mi * minv * oi | (mi, oi, minv) <- zip3 mis os invs]
    print (t `mod` m)
    print [t `mod` n | n <- ns]
