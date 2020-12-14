{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Day14.Part2 where

import Common.Utils
import Text.Parsec
    ( digit, endOfLine, oneOf, string, count, endBy, (<|>), many, try )
import Common.FileLoading
import Control.Monad.State
import Data.Bits
import qualified Data.Map as M

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
    mem :: M.Map Integer Integer
} deriving Show

initState = ExecState (replicate 36 'X') M.empty

---

toBits :: Integer -> String
toBits n = [if (n .&. (2^i)) > 0 then '1' else '0' | i <- [35,34..0]]

fromBits :: String -> Integer
fromBits s = sum [if c == '1' then 2^i else 0 | (c, i) <- zip s [35,34..0]]

possibleAddressess :: String -> String -> [String]
possibleAddressess [] [] = [""]
possibleAddressess ('0':mt) (h:t) = map (h:) $ possibleAddressess mt t
possibleAddressess ('1':mt) (_:t) = map ('1':) $ possibleAddressess mt t
possibleAddressess ('X':mt) (_:t) = let pa = possibleAddressess mt t in map ('0':) pa <> map ('1':) pa

getAddressess :: String -> Integer -> [Integer]
getAddressess mask address = map fromBits $ possibleAddressess mask (toBits address)

processInstruction :: (MonadState ExecState m) => Instruction -> m ()
processInstruction (Mask mask) = modify (\m@ExecState{mask=_} -> m{mask=mask})
processInstruction (Mem address value) = do
    mask <- gets mask
    forM_ (getAddressess mask address) $ \address' -> do
        modify (\m@ExecState{mem} -> m{mem=M.insert address' value mem})

solution = do
    instructions <- readParsed (Day 14) parseInstructions
    result <- flip execStateT initState $ forM_ instructions processInstruction
    print "Printing result"
    print $ M.size $ mem result
    print $ sum $ map snd $ M.toList $ mem result