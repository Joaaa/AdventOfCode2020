{-# LANGUAGE FlexibleContexts #-}
module Day15.Part1 where

import qualified Data.Map as M
import Control.Monad.State

inputs l = M.fromList $ zip l [0..]

startState = Numbers 6 0 (inputs [5,1,9,18,13,8])

data Numbers = Numbers {
    turn :: Int,
    last :: Int,
    history :: M.Map Int Int
} deriving Show

nextNumber :: (MonadState Numbers m) => m Int
nextNumber = do
    Numbers t l h <- get
    case h M.!? l of
        Just p -> return $ t - p
        Nothing -> return 0

processNext :: (MonadState Numbers m, MonadIO m) => m ()
processNext = do
    next <- nextNumber
    -- get >>= liftIO . print
    modify (\(Numbers t l h) -> Numbers (t+1) next (M.insert l t h))


solution = do
    r <- flip execStateT startState $ replicateM_ (2020 - turn startState - 1) processNext
    print $ Day15.Part1.last r