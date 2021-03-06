module GameIO (runGame, runGameWithPC) where

import Data.Maybe
import GameWinner
import MiniMax
import PutMarkWithState
import Text.Read (readMaybe)

type GameLog = (Game, [Int])

runGameWithPC :: IO GameLog
runGameWithPC = do
  let g = startGameWithPlayer 3 "o" "x"
  let Just fg = g
  putBoardStr fg
  putStrLn "Type number that you want to put"
  n <- getNumLoop []
  let Just b = g >>= playGame n
  putBoardStr b
  (gm, xs) <- getNumPCTurn (Just b, [n])
  putStrLn $ if length xs == 9 then drawAndExit gm else let Just (currentP, nextP, gs, b) = gm in "winner is : " ++ currentP
  return (g, xs)
  where
    drawAndExit mgs = let (currentP, nextP, gs, b) = justGS mgs in "draw"
    justGS (Just gs) = gs

getNumPCTurn :: GameLog -> IO GameLog
getNumPCTurn (g, xs) = do
  let Just (m1, m2, _, gb) = g
  let n = findNicePos m1 m2 gb
  putStrLn $ m1 ++ " : " ++ show n
  let b = g >>= playGame n
  if isNothing b || isWinner b then stopCall g n else retry b n >>= getNumPlayerTurn
  where
    stopCall b n = do
      let Just gb = if isNothing (b >>= playGame n) then b else b >>= playGame n
      putBoardStr gb
      return (b, n : xs) :: IO GameLog
    retry b n = do
      let Just gb = b
      putBoardStr gb
      return (b, n : xs) :: IO GameLog

getNumPlayerTurn :: GameLog -> IO GameLog
getNumPlayerTurn (g, xs) = do
  putStrLn "Type number that you want to put"
  n <- getNumLoop xs
  let b = g >>= playGame n
  -- print b
  if isNothing b || isWinner b then stopCall g n else retry b n >>= getNumPCTurn
  where
    stopCall b n = do
      let Just gb = if isNothing (b >>= playGame n) then b else b >>= playGame n
      putBoardStr gb
      return (b, n : xs) :: IO GameLog
    retry b n = do
      let Just gb = b
      putBoardStr gb
      return (b, n : xs) :: IO GameLog

runGame :: IO GameLog
runGame = do
  let g = startGameWithPlayer 3 "o" "x"
  let Just fg = g
  putBoardStr fg
  putStrLn "Type number that you want to put"
  n <- getNumLoop []
  let Just b = g >>= playGame n
  putBoardStr b
  (gm, xs) <- getNum (Just b, [n])
  putStrLn $ if length xs == 9 then drawAndExit gm else let Just (currentP, nextP, gs, b) = gm in "winner is : " ++ currentP
  return (g, xs)
  where
    drawAndExit mgs = let (currentP, nextP, gs, b) = justGS mgs in "draw"
    justGS (Just gs) = gs

getNum :: GameLog -> IO GameLog
getNum (g, xs) = do
  putStrLn "Type number that you want to put"
  n <- getNumLoop xs
  let b = g >>= playGame n
  -- print b
  if isNothing b || isWinner b then stopCall g n else retry b n >>= getNum
  where
    stopCall b n = do
      let Just gb = if isNothing (b >>= playGame n) then b else b >>= playGame n
      putBoardStr gb
      return (b, n : xs) :: IO GameLog
    retry b n = do
      let Just gb = b
      putBoardStr gb
      return (b, n : xs) :: IO GameLog

getNumLoop :: [Int] -> IO Int
getNumLoop xs = do
  str <- getLine
  if isNothing (maybeNum str) || let Just n = maybeNum str in (n `elem` xs || (n < 1 || 9 < n)) then tryAgain else let Just num = maybeNum str in return num
  where
    maybeNum str = do
      readMaybe str :: Maybe Int
    tryAgain = do
      putStrLn "try again"
      getNumLoop xs
