module Common.FileLoading where

import Text.Parsec

data Day = Day Int | DayPart Int Int deriving Show

readInput :: Day -> IO String
readInput (Day n) = readFile ("./res/InputsDay" <> show n <> ".txt")
readInput (DayPart n p) = readFile ("./res/InputsDay" <> show n <> "Part" <> show p <> ".txt")

readLines :: Day -> IO [String]
readLines day = lines <$> readInput day

readParsed :: Day -> Parsec String () a -> IO a
readParsed day parser = do
    input <- readInput day
    case parse parser (show day) input of
        Left e -> error $ show e
        Right r -> return r