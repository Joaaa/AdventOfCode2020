module Day22.Part1 where

import Common.Utils
import Text.Parsec
import Control.Applicative (Applicative(liftA2))
import Common.FileLoading
type Deck = [Int]

type GameState = (Deck, Deck)

parseDeck :: Parser Deck
parseDeck = string "Player " >> digit >> char ':' >> endOfLine >> endBy (read <$> many1 alphaNum) endOfLine <* endOfLine 

parseGameState :: Parser GameState
parseGameState = liftA2 (,) parseDeck parseDeck

playRound :: GameState -> GameState
playRound (h1:t1, h2:t2) | h1 > h2 = (t1 <> [h1, h2], t2)
playRound (h1:t1, h2:t2) | h2 > h1 = (t1, t2 <> [h2, h1])

gameOver :: GameState -> Bool
gameOver ([], _) = True
gameOver (_, []) = True
gameOver (_, _) = False

solution = do
    startState <- readParsed (Day 22) parseGameState
    let endState = head $ dropWhile (not . gameOver) $ iterate playRound startState
    print startState
    print endState
    let winningCards = (\(c1, c2) -> if null c1 then c2 else c1) endState
    print $ sum [c * m | (c, m) <- zip (reverse winningCards) [1..]]