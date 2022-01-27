module PutMark (genBoard, putMark, Board) where

import Text.Read (readMaybe)

type Mark = String

type DefaultMark = String

type Width = Int

type Pos = Int

type Board = [String]

type BoardMap = [Int]

genBoardMap :: Width -> BoardMap
genBoardMap w = [1 .. (w * w)]

genBoard :: Width -> Board
genBoard w = map show $ genBoardMap w

putMark :: Mark -> Pos -> Board -> Board
putMark m p = map posToMark
  where
    posToMark n
      | readMaybe n == Just p = m
      | otherwise = n
