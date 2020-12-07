module Day4.Part2Parsec where

import Day4.Part1Parsec (parsePassports,  Passport(Passport), KV(KV))
import Common.FileLoading
import Text.Parsec
import Data.Functor

data Rule = Rule {
    key :: String,
    valueMatcher :: Parsec String () Bool
}

rules :: [Rule]
rules = [
    Rule "byr" (do
            x <- read <$> count 4 digit
            return $ x >= 1920 && x <= 2002
        ),
    Rule "iyr" (do
            x <- read <$> count 4 digit
            return $ x >= 2010 && x <= 2020
        ),
    Rule "eyr" (do
            x <- read <$> count 4 digit
            return $ x >= 2020 && x <= 2030
        ),
    Rule "hgt" (((\x -> x >= 150 && x <= 193) . read <$> try (many digit <* string "cm")) <|> ((\x -> x >= 59 && x <= 76) . read <$> (many digit <* string "in"))),
    Rule "hcl" (char '#' >> count 6 alphaNum $> True),
    Rule "ecl" (choice (map (try . string) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) $> True),
    Rule "pid" (count 9 digit $> True)
    ]

isValid (Passport kvs) = all (\(Rule key rule) -> any (\(KV k v) -> k == key && parse (rule <* eof) "" v == Right True) kvs) rules

solution = do
    lines <- readLines (Day 4)
    let passports = parse parsePassports "" (unlines lines)
    case passports of
        Left error -> print error
        Right passports -> print $ length $ filter isValid passports