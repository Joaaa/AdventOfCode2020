{-# LANGUAGE FlexibleContexts #-}
module Day12.Part2 where

import Common.FileLoading
import Text.Parsec
import Common.Utils
import Data.Functor
import Control.Monad.State

data Direction = N | E | S | W deriving Show
data Instruction = Move Direction Int | Turn Int | Forward Int deriving Show
type Position = (Int, Int)

turn :: Int -> Position -> Position
turn 0 p = p
turn n (x, y) = turn (n-1) (y, -x)

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
    pos :: Position,
    wp :: Position
}

applyInstruction :: (MonadState Ferry m) => Instruction -> m ()
applyInstruction (Move d n) = modify (\f@Ferry{wp=p} -> f{wp=move d n p})
applyInstruction (Turn n) = modify (\f@Ferry{wp=p} -> f{wp=turn n p})
applyInstruction (Forward n) = modify (\f@Ferry{pos=(x,y), wp=(dx,dy)} -> f{pos=(x+n*dx,y+n*dy)})

solution = do
    instructions <- readParsed (Day 12) (endBy parseInstruction endOfLine)
    r <- flip execStateT (Ferry (0, 0) (10, 1)) $ forM_ instructions applyInstruction
    let md = (\(a, b) -> abs a + abs b) $ pos r
    print md
