module Day4.Part1 where

import Common.FileLoading
import Common.Utils

newtype Passport = Passport [KV] deriving Show
data KV = KV String String deriving Show

parsePassport :: String -> Passport
parsePassport s = Passport $ map toKV $ words s where
    toKV s = let [k, v] = split ":" s in KV k v

isValid (Passport kvs) = all (`elem` map (\(KV k _) -> k) kvs) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

solution = do
    lines <- readLines (Day 4)
    let passports = map (parsePassport . concatMap (<> " ")) $ split [""] lines
    print $ length $ filter isValid passports
