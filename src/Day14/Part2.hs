{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
module Day14.Part2 where

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

possibleAddressess :: String -> String -> [String]
possibleAddressess [] [] = [""]
possibleAddressess ('0':mt) (h:t) = map (h:) $ possibleAddressess mt t
possibleAddressess ('1':mt) (_:t) = map ('1':) $ possibleAddressess mt t
possibleAddressess ('X':mt) (_:t) = let pa = possibleAddressess mt t in map ('0':) pa <> map ('1':) pa

getAddressess :: String -> Integer -> [Integer]
getAddressess mask address = map fromBits $ possibleAddressess mask (toBits address)

setMem :: Integer -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
setMem address value [] = [(address, value)]
setMem address value ((a, _):t) | a == address = (a, value) : t
setMem address value (h:t) = h : setMem address value t

processInstruction :: (MonadState ExecState m, MonadIO m) => Instruction -> m ()
processInstruction (Mask mask) = modify (\m@ExecState{mask=_} -> m{mask=mask})
processInstruction (Mem address value) = do
    liftIO $ print (Mem address value)
    mask <- gets mask
    liftIO $ print $ length $ getAddressess mask address
    forM_ (getAddressess mask address) $ \address' -> do
        liftIO $ print $ "Setting address " <> show address'
        modify (\m@ExecState{mem} -> m{mem=setMem address' value mem})

solution = do
    instructions <- readParsed (Day 14) parseInstructions
    result <- flip execStateT initState $ forM_ instructions processInstruction
    print "Printing result"
    print $ sum $ map snd $ mem result