module PutMarkSpec where

import PutMark
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "board" $ do
  it "should be formatted" $ do
    let board = genBoard 3
    board `shouldBe` ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

  it "should be changed" $ do
    let board = putMark "x" 5 $ genBoard 3
    board `shouldBe` ["1", "2", "3", "4", "x", "6", "7", "8", "9"]

  it "should be able to put 1 times" $ do
    let board = genBoard 3
    let puted = putMark "x" 1 board
    puted `shouldBe` ["x", "2", "3", "4", "5", "6", "7", "8", "9"]

  it "should be able to put 3 times" $ do
    let board = genBoard 3
    let puted = putMark "x" 1 $ putMark "o" 3 $ putMark "x" 5 board
    puted `shouldBe` ["x", "2", "o", "4", "x", "6", "7", "8", "9"]
