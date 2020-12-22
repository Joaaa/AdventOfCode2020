module Day22.Part2 where

import Common.Utils
import Text.Parsec
import Control.Applicative (Applicative(liftA2))
import Common.FileLoading
import qualified Data.Set as S
type Deck = [Int]

type GameState = (Deck, Deck, S.Set Integer, Bool)

parseDeck :: Parser Deck
parseDeck = string "Player " >> digit >> char ':' >> endOfLine >> endBy (read <$> many1 alphaNum) endOfLine <* endOfLine 

parseGameState :: Parser GameState
parseGameState = do
    d1 <- parseDeck
    d2 <- parseDeck
    return (d1, d2, S.empty, False)

player1Winner :: GameState -> Bool
player1Winner (h1:t1, h2:t2, prevs, stop) | h1 <= length t1 && h2 <= length t2 = let (ed1, _, _, stop) = playUntilWin (take h1 t1, take h2 t2, S.empty, False) in stop || not (null ed1)
player1Winner (h1:t1, h2:t2, prevs, stop) = h1 > h2

deckToInteger :: Deck -> Integer
deckToInteger d = sum [(fromIntegral c - 1) * (50 ^ n) | (c, n) <- zip d [0..]]

playRound :: GameState -> GameState
playRound gs@(d1@(h1:t1), d2@(h2:t2), prevs, stop) | deckToInteger d1 `S.member` prevs = (d1, d2, prevs, True)
playRound gs@(d1@(h1:t1), d2@(h2:t2), prevs, stop) = if player1Winner gs then (t1 <> [h1, h2], t2, S.insert (deckToInteger d1) prevs, stop) else (t1, t2 <> [h2, h1], prevs, stop)

gameOver :: GameState -> Bool
gameOver (_, _, _, True) = True
gameOver ([], _, _, _) = True
gameOver (_, [], _, _) = True
gameOver (_, _, _, _) = False

playUntilWin :: GameState -> GameState
playUntilWin startState = head $ dropWhile (not . gameOver) $ iterate playRound startState

solution = do
    startState <- readParsed (Day 22) parseGameState
    let (d1, d2, _, stop) = playUntilWin startState
    let winningCards = if stop || not (null d1) then d1 else d2
    print $ sum [c * m | (c, m) <- zip (reverse winningCards) [1..]]