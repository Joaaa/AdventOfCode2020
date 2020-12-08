module Day8.Part1 where

import Text.Parsec
import Data.Functor
import Common.Utils
import Common.FileLoading
import Control.Monad
import Control.Monad.State
data Instruction = Nop Int | Acc Int | Jmp Int deriving Show

parseInstruction :: Parser Instruction
parseInstruction = choice [string "nop" $> Nop, string "acc" $> Acc, string "jmp" $> Jmp] <*> (space *> (parse <$> many (oneOf "+-0123456789"))) where
    parse ('+':s) = parse s
    parse s = read s

parseInstructions :: Parser [Instruction]
parseInstructions = sepBy parseInstruction endOfLine <* eof

data RunState = RunState {
    visited :: [Int],
    pc :: Int,
    acc :: Int
} deriving Show

run :: [Instruction] -> StateT RunState IO ()
run instructions = do
    RunState visited pc acc <- get
    when (pc `notElem` visited) $ do
        case instructions !! pc of
            Nop _ -> put $ RunState (pc:visited) (pc+1) acc
            Acc n -> put $ RunState (pc:visited) (pc+1) (acc+n)
            Jmp n -> put $ RunState (pc:visited) (pc+n) acc
        run instructions

solution :: IO ()
solution = do
    instructions <- readParsed (Day 8) parseInstructions
    RunState _ _ acc <- execStateT (run instructions) (RunState [] 0 0)
    print acc
