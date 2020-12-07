module Day6.Part2 where

import Common.FileLoading
import Text.Parsec
import Data.Functor

inputParser :: Parsec String () [[[Char]]]
inputParser = many (many1 (many1 alphaNum <* (newline $> () <|> eof)) <* optional newline)

countAllYes :: [String] -> Int
countAllYes l = length $ filter (== True) $ foldl (zipWith (&&)) (repeat True) [map (`elem` sl) ['a'..'z']| sl <- l]

solution :: IO ()
solution = do
    inputs <- readParsed (DayPart 6 2) inputParser
    print $ sum $ map countAllYes inputs
    