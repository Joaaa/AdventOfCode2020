{-# LANGUAGE FlexibleContexts #-}
module Day12.Part1 where

import Common.FileLoading
import Text.Parsec
import Common.Utils
import Data.Functor
import Control.Monad.State

data Direction = N | E | S | W deriving Show
data Instruction = Move Direction Int | Turn Int | Forward Int deriving Show
type Position = (Int, Int)

turn :: Direction -> Int -> Direction
turn d 0 = d
turn N n = turn E (n-1)
turn E n = turn S (n-1)
turn S n = turn W (n-1)
turn W n = turn N (n-1)

move :: Direction -> Int -> Position -> Position
move N n (x, y) = (x, y+n)
move E n (x, y) = (x+n, y)
move S n (x, y) = (x, y-n)
move W n (x, y) = (x-n, y)


parseNSEW :: Parser Instruction
parseNSEW = choice [char 'N' $> N, char 'E' $> E, char 'S' $> S, char 'W' $> W] >>= \d -> Move d . read <$> many digit

parseLR :: Parser Instruction
parseLR = do
    c <- oneOf "LR"
    n <- read <$> many digit
    return $ Turn $ ((if c == 'R' then 1 else -1) * (n `div` 90) + 4) `mod` 4

parseF :: Parser Instruction
parseF = char 'F' >> Forward . read <$> many digit

parseInstruction :: Parser Instruction
parseInstruction = parseNSEW <|> parseLR <|> parseF

data Ferry = Ferry {
    pos :: (Int, Int),
    dir :: Direction
}

applyInstruction :: (MonadState Ferry m) => Instruction -> m ()
applyInstruction (Move d n) = modify (\f@Ferry{pos=p} -> f{pos=move d n p})
applyInstruction (Turn n) = modify (\f@Ferry{dir=d} -> f{dir=turn d n})
applyInstruction (Forward n) = modify (\f@Ferry{pos=p, dir=d} -> f{pos=move d n p})

solution = do
    instructions <- readParsed (Day 12) (endBy parseInstruction endOfLine)
    r <- flip execStateT (Ferry (0, 0) E) $ forM_ instructions applyInstruction
    let md = (\(a, b) -> abs a + abs b) $ pos r
    print md
