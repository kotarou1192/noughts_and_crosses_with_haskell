module PutMarkWithState (startGameWithPlayer, playGame, putBoardStr) where

import Data.Maybe
import GameState
import PutMark

type CurrentPMark = String

type NextPMark = String

type BoardWidth = Int

type State = (CurrentPMark, NextPMark, GameState, Board)

type Pos = Int

type Game = Maybe State

startGameWithPlayer :: BoardWidth -> CurrentPMark -> NextPMark -> Game
startGameWithPlayer n m1 m2 = Just (m1, m2, startGame, genBoard n)

playGame :: Pos -> State -> Game
playGame p (m1, m2, gs, b)
  | isNothing gs = Nothing
  | otherwise = Just (m2, m1, gs >>= processGame, putMark m1 p b)

putBoardStr :: State -> IO ()
putBoardStr (_, _, _, b) = printBoard b

printBoard :: Board -> IO ()
printBoard b = do
  let l1 = take 3 b
  putStrLn (concat l1)
  let b2 = drop 3 b
  let l2 = take 3 b2
  putStrLn (concat l2)
  let b3 = drop 3 b2
  putStrLn (concat b3)
  return ()
