module MiniMax (playGameWithPC, putNicePos, findNicePos) where

import GameState
import GameWinner
import PutMark

type CurrentPMark = String

type NextPMark = String

type IsMe = Bool

type State = (CurrentPMark, NextPMark, GameState, Board)

type Game = Maybe State

type Score = Int

type Pos = Int

playGameWithPC :: State -> Game
playGameWithPC (m1, m2, gs, b)
  | let Just (t, _) = gs in t >= 9 = Nothing
  | otherwise = Just (m2, m1, gs >>= processGame, putNicePos m1 m2 b)

putNicePos :: String -> String -> Board -> Board
putNicePos m1 m2 b = putMark m1 (findNicePos m1 m2 b) b

findNicePos :: String -> String -> Board -> Int
findNicePos m1 m2 b = snd $ findNext True b m1 m2

findNext :: IsMe -> Board -> String -> String -> (Score, Pos)
findNext me b m1 m2 = if me then maximum f else minimum f
  where
    f = map (\x -> (fst (minimax me (putMark m1 x b) x m1 m2), x)) (canPut b)

fi me b m1 m2 = map (\x -> minimax me (putMark m1 x b) x m1 m2) (canPut b)

minimax :: IsMe -> Board -> Pos -> String -> String -> (Score, Pos)
minimax me b i m1 m2
  | isWinner' b && me = (1, i)
  | isWinner' b && not me = (-1, i)
  | null (canPut b) = (0, i)
  | otherwise = findNext (not me) b m2 m1

canPut :: Board -> [Int]
canPut b = map readInt (dropMarks b)
  where
    readInt s = read s :: Int
    dropMarks gb = filter (\a -> a /= "o" && a /= "x") gb
