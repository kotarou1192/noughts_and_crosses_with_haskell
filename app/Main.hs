module Main where

import Control.Monad (forever)
-- import qualified PutMarkWithState

import Data.Maybe
import GameIO
import Text.Read (readMaybe)

main :: IO ()
main = do
  runGame
  return ()
