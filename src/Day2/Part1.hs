module Day2.Part1 where

data Password = Password Int Int Char String deriving Show

readInputs :: IO [String]
readInputs = lines <$> readFile "./res/InputsDay2.txt"

parseInput :: String -> Password
parseInput input = Password min max c pw where
    pw = drop 2 $ dropWhile (/= ':') input
    rule = takeWhile (/= ':') input
    bounds = takeWhile (/= ' ') rule
    c = dropWhile (/= ' ') rule !! 1
    min = read $ takeWhile (/= '-') bounds
    max = read $ drop 1 $ dropWhile (/= '-') bounds

isValid (Password min max c pw) = min <= cnt && cnt <= max where 
    cnt = length $ filter (== c) pw

countValid :: [Password] -> Int
countValid = length . filter isValid

solution = do
    passwords <- map parseInput <$> readInputs
    print $ countValid passwords
