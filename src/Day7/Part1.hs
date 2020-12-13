{-# LANGUAGE TupleSections #-}
module Day7.Part1 where

import Common.FileLoading
import Common.Utils
import Text.Parsec
import Data.Functor
import Control.Monad.State
import Data.Maybe

data Bag = Bag String String deriving (Show, Eq)

data Rule = Rule Bag [(Int, Bag)] deriving Show

parseBag :: Parser Bag
parseBag = do
    color1 <- many1 alphaNum
    space
    color2 <- many1 alphaNum
    string " bag" >> optional (char 's')
    return $ Bag color1 color2

parseBagAmount = do
    n <- read <$> many digit
    space
    (n,) <$> parseBag

parseRule = do
    bag <- parseBag
    string " contain "
    bags <- (try (string "no other bag" >> optional (char 's')) $> []) <|> sepBy parseBagAmount (string ", ")
    return $ Rule bag bags

matches :: Rule -> [Bag] -> Bool
matches (Rule bag []) _ = False
matches (Rule bag bagAmounts) bags = any bagAmountMatches bagAmounts where
    bagAmountMatches (n, b) = b `elem` bags

newBags :: [Rule] -> [Bag] -> [Bag]
newBags rules bags = mapMaybe (\r@(Rule bag _) -> if bag `elem` bags then Nothing else if matches r bags then Just bag else Nothing) rules

solution = print 123
    -- rules <- readParsed (Day 7) (endBy parseRule (char '.' >> endOfLine))
    -- forM_ (newBags rules [Bag "shiny" "gold"]) print
    -- let results = (`evalState` [Bag "shiny" "gold"]) $ replicateM 10 $ do
    --     bags <- get
    --     let new = newBags rules bags
    --     modify (<> new)
    --     get
    -- print $ map (pred . length) results

