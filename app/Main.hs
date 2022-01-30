module Main where

import Control.Monad (forever)
-- import qualified PutMarkWithState

import Data.Maybe
import GameIO
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "battle to cpmputer player? [y/n]"
  str <- getLine
  if str == "y" then runGameWithPC else runGame
  putStrLn "thank you for playing, bye!"
  return ()
