{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Day19.Part2 where

import Common.Utils
import Text.Parsec
import Control.Applicative (liftA2)
import Common.FileLoading
import qualified Data.Map as M
import qualified Data.Vector as V
import Control.Monad.RWS
import Data.Functor
import Data.Maybe
import Data.Either

data Rule = RChar Char | Concat [[Int]] deriving Show
type IndexedRule = (Int, Rule)

type Message = String

parseChar :: Parser Rule
parseChar = RChar <$> (char '"' *> alphaNum <* char '"')

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseConcat :: Parser Rule
parseConcat = Concat <$> sepBy (endBy parseInt $ optional $ char ' ') (string "| ")

parseRule :: Parser IndexedRule
parseRule = liftA2 (,) (parseInt <* string ": ") (parseChar <|> parseConcat)

parseMessage :: Parser Message
parseMessage = many alphaNum 

parseInput :: Parser ([IndexedRule], [Message])
parseInput = liftA2 (,) (endBy parseRule endOfLine <* endOfLine) (sepBy parseMessage endOfLine)

matches :: M.Map Int Rule -> Int -> String -> [String]
matches _ _ [] = []
matches ruleMap 8 message = concat $ takeWhile (not . null) $ drop 1 $ iterate (concatMap (matches ruleMap 42)) [message]
matches ruleMap 11 message = concatMap (uncurry apply31s) $ zip [1..] $ takeWhile (not . null) $ drop 1 $ iterate (concatMap (matches ruleMap 42)) [message]
    where
        apply31s i ms = iterate (concatMap (matches ruleMap 31)) ms !! i
matches ruleMap i message = case ruleMap M.! i of
    RChar c -> [tail message | c == head message]
    Concat l -> concatMap (matches' [message]) l
    where
        matches' ms [] = ms
        matches' ms (h:t) = matches' (concatMap (matches ruleMap h) ms) t

matchesFully ruleMap i message = "" `elem` matches ruleMap i message

solution = do
    (rules, messages) <- readParsed (Day 19) parseInput
    let ruleMap = M.fromList rules
    print $ length $ filter (matchesFully ruleMap 0) messages