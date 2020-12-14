{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Day14.Part1 where

import Common.Utils
import Text.Parsec
    ( digit, endOfLine, oneOf, string, count, endBy, (<|>), many, try )
import Common.FileLoading
import Control.Monad.State
import Data.Bits

data Instruction = Mask String | Mem Integer Integer deriving Show

parseMask :: Parser Instruction
parseMask = Mask <$> (string "mask = " *> count 36 (oneOf "10X"))

parseMem :: Parser Instruction
parseMem = do
    string "mem["
    address <- read <$> many digit
    string "] = "
    Mem address . read <$> many digit

parseInstruction = try parseMask <|> parseMem

parseInstructions = endBy parseInstruction endOfLine

---

data ExecState = ExecState {
    mask :: String,
    mem :: [(Integer, Integer)]
} deriving Show

initState = ExecState (replicate 36 'X') []

---

toBits :: Integer -> String
toBits n = [if (n .&. (2^i)) > 0 then '1' else '0' | i <- [35,34..0]]

fromBits :: String -> Integer
fromBits s = sum [if c == '1' then 2^i else 0 | (c, i) <- zip s [35,34..0]]

applyMask :: String -> Integer -> Integer
applyMask mask value = fromBits [if c == 'X' then b else c | (c, b) <- zip mask $ toBits value]

setMem :: Integer -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
setMem address value [] = [(address, value)]
setMem address value ((a, _):t) | a == address = (a, value) : t
setMem address value (h:t) = h : setMem address value t

processInstruction :: (MonadState ExecState m) => Instruction -> m ()
processInstruction (Mask mask) = modify (\m@ExecState{mask=_} -> m{mask=mask})
processInstruction (Mem address value) = modify (\m@ExecState{mask, mem} -> m{mem=setMem address (applyMask mask value) mem})

solution = do
    instructions <- readParsed (Day 14) parseInstructions
    result <- flip execStateT initState $ forM_ instructions processInstruction
    print $ sum $ map snd $ mem result