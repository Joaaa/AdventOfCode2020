module Day8.Part2 where

import Day8.Part1 hiding (run, solution)
import Control.Monad.State
import Common.FileLoading
import Data.Maybe
import Data.Foldable

run :: [Instruction] -> StateT RunState IO Bool
run instructions = do
    RunState visited pc acc <- get
    if pc >= length instructions
        then return True
        else if pc `notElem` visited then do
            case instructions !! pc of
                Nop _ -> put $ RunState (pc:visited) (pc+1) acc
                Acc n -> put $ RunState (pc:visited) (pc+1) (acc+n)
                Jmp n -> put $ RunState (pc:visited) (pc+n) acc
            run instructions
            else return False

switch :: Instruction -> Maybe Instruction
switch (Nop n) = Just $ Jmp n
switch (Jmp n) = Just $ Nop n
switch (Acc _) = Nothing

replaceNth :: Int -> [Instruction] -> Maybe [Instruction]
replaceNth i instructions = case switch (instructions !! i) of
    Just ins -> Just [if j == i then ins else instructions !! j | j <- [0..length instructions-1]]
    Nothing -> Nothing

solution :: IO ()
solution = do
    instructions <- readParsed (Day 8) parseInstructions
    let candidates = mapMaybe (`replaceNth` instructions) [0..length instructions-1]
    results <- forM candidates $ \c -> runStateT (run c) (RunState [] 0 0)
    print $ find fst results
