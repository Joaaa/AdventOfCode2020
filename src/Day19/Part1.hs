module Day19.Part1 where

import Common.Utils
import Text.Parsec
import Control.Applicative (liftA2)
import Common.FileLoading
import qualified Data.Map as M
import qualified Data.Vector as V

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

getPossibilities :: M.Map Int Rule -> Int -> [String]
getPossibilities ruleMap i = case ruleMap M.! i of
    RChar c -> [[c]]
    Concat l -> do
        cRule <- l
        map concat $ mapM (getPossibilities ruleMap) cRule

solution = do
    (rules, messages) <- readParsed (Day 19) parseInput
    let ruleMap = M.fromList rules
    let possibilities = V.fromList $ getPossibilities ruleMap 0
    print $ length $ filter (`V.elem` possibilities) messages
    return ()