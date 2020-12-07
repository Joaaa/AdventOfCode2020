module Day4.Part2 where

import Data.Bifunctor
import Day4.Part1 ( Passport(Passport), KV(KV), parsePassport )
import Common.FileLoading
import Common.Utils

data Rule = Rule {
    key :: String,
    valueMatcher :: String -> Bool
}

rules = [
    Rule "byr" (\s -> isNumber s && (\x -> x >= 1920 && x <= 2002) (read s)),
    Rule "iyr" (\s -> isNumber s && (\x -> x >= 2010 && x <= 2020) (read s)),
    Rule "eyr" (\s -> isNumber s && (\x -> x >= 2020 && x <= 2030) (read s)),
    Rule "hgt" ((\(x, unit) -> if unit == "cm" then x >= 150 && x <= 193 else unit == "in" && x >= 59 && x <= 76) . first read . (\s -> splitAt (length s - 2) s)),
    Rule "hcl" (\s -> length s == 7 && head s == '#' && all (`elem` "01236456789abcdefghijklmnopqrstuvwxyz") (tail s)),
    Rule "ecl" (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
    Rule "pid" (\s -> length s == 9 && isNumber s)
    ]

isNumber = all (`elem` "0123456789")

isValid (Passport kvs) = all (\(Rule key matches) -> any (\(KV k v) -> k == key && matches v) kvs) rules

solution = do
    lines <- readLines (Day 4)
    let passports = map (parsePassport . concatMap (<> " ")) $ split [""] lines
    print $ length $ filter isValid passports