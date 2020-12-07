module Day4.Part1Parsec where

import Common.FileLoading
import Common.Utils
-- import Text.Parsec.Char
import Text.Parsec
import Control.Monad

newtype Passport = Passport [KV] deriving Show
data KV = KV String String deriving Show

parseKey = many1 alphaNum

parseValue = many1 (alphaNum <|> char '#')

parseKV = do
    k <- parseKey
    char ':'
    KV k <$> parseValue

parsePassport = many1 (parseKV <* (space <|> newline))

parsePassports :: Parsec String () [Passport]
parsePassports = map Passport <$> sepBy parsePassport newline <* eof

isValid (Passport kvs) = all (`elem` map (\(KV k _) -> k) kvs) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

solution = do
    lines <- readLines (Day 4)
    let passports = parse parsePassports "" (unlines lines)
    case passports of
        Left error -> print error
        Right passports -> do
            forM_ passports print
            print $ length $ filter isValid passports