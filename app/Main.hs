module Main where

import Control.Monad (forever)
-- import qualified PutMarkWithState

import Data.Maybe
import PutMarkWithState
import Text.Read (readMaybe)

main :: IO ()
main = do
  let g = startGameWithPlayer 3 "o" "x"
  putStrLn "Tyne number that you want to put"
  n <- getNumLoop
  let Just b = g >>= playGame n
  putBoardStr b

getNumLoop = do
  str <- getLine
  if isNothing $ maybeNum str then tryAgain else let Just num = maybeNum str in return num
  where
    maybeNum str = do
      readMaybe str :: Maybe Int
    tryAgain = do
      putStrLn "try again"
      getNumLoop
