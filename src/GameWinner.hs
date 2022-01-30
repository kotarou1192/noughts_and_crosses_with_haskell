module GameWinner (isWinner, isAlignRight, isAlignDown, isAlignCross) where

import PutMark
import PutMarkWithState

type Index = Int

type Mark = String

isWinner :: Game -> Bool
isWinner Nothing = False
isWinner (Just (_, _, _, b)) = any (isAlignRight b) [0, 3, 6] || any (isAlignDown b) [0, 1, 2] || any (isAlignCross b) [0, 2]

isAlignRight :: Board -> Index -> Bool
isAlignRight b i
  | (i + 1) `mod` 3 == 0 = True
  | b !! i == b !! (i + 1) = isAlignRight b (i + 1)
  | otherwise = False

isAlignDown :: Board -> Index -> Bool
isAlignDown b i
  | i >= 6 = True
  | b !! i == b !! (i + 3) = isAlignDown b (i + 3)
  | otherwise = False

isAlignCross :: Board -> Index -> Bool
isAlignCross b i
  | i == 0 = isAlignCrossRight b i
  | i == 2 = isAlignCrossLeft b i
  | otherwise = False

isAlignCrossRight :: Board -> Index -> Bool
isAlignCrossRight b i
  | i == 8 = True
  | b !! i == b !! (i + 4) = isAlignCrossRight b (i + 4)
  | otherwise = False

isAlignCrossLeft :: Board -> Index -> Bool
isAlignCrossLeft b i
  | i == 6 = True
  | b !! i == b !! (i + 2) = isAlignCrossLeft b (i + 2)
  | otherwise = False
