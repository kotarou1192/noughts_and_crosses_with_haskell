module GameWinnerSpec where

import GameWinner
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "winner" $ do
  describe "align right" $ do
    it "should be True" $ do
      let b = ["o", "o", "o", "4", "5", "6", "7", "8", "9"]
      isAlignRight b 0 `shouldBe` True

    it "should be True" $ do
      let b = ["o", "x", "o", "x", "x", "x", "7", "8", "9"]
      isAlignRight b 3 `shouldBe` True

    it "should be True" $ do
      let b = ["o", "o", "o", "4", "5", "6", "o", "o", "o"]
      isAlignRight b 6 `shouldBe` True

    it "should be False" $ do
      let b = ["o", "x", "o", "4", "5", "6", "7", "8", "9"]
      isAlignRight b 0 `shouldBe` False

    it "should be True" $ do
      let b = ["o", "x", "o", "o", "o", "o", "7", "8", "9"]
      isAlignRight b 3 `shouldBe` True

    it "should be False" $ do
      let b = ["o", "x", "x", "x", "x", "o", "7", "8", "9"]
      isAlignRight b 0 `shouldBe` False

    it "should be False" $ do
      let b = ["o", "o", "x", "o", "5", "6", "x", "8", "9"]
      isAlignDown b 0 `shouldBe` False

  describe "align down" $ do
    it "should be True" $ do
      let b = ["o", "o", "x", "o", "5", "6", "o", "8", "9"]
      isAlignDown b 0 `shouldBe` True

    it "should be False" $ do
      let b = ["o", "o", "x", "o", "5", "x", "x", "8", "x"]
      isAlignDown b 0 `shouldBe` False

    it "should be True" $ do
      let b = ["o", "o", "x", "x", "o", "6", "o", "o", "9"]
      isAlignDown b 1 `shouldBe` True

    it "should be False" $ do
      let b = ["o", "o", "x", "o", "o", "x", "x", "8", "x"]
      isAlignDown b 1 `shouldBe` False

    it "should be True" $ do
      let b = ["o", "o", "x", "x", "5", "x", "o", "8", "x"]
      isAlignDown b 2 `shouldBe` True

    it "should be False" $ do
      let b = ["o", "o", "x", "o", "x", "x", "x", "o", "o"]
      isAlignDown b 2 `shouldBe` False

  describe "align cross" $ do
    it "should be True" $ do
      let b = ["o", "o", "x", "x", "o", "6", "o", "8", "o"]
      isAlignCross b 0 `shouldBe` True

    it "should be True" $ do
      let b = ["o", "o", "x", "o", "x", "x", "x", "8", "x"]
      isAlignCross b 2 `shouldBe` True

    it "should be False" $ do
      let b = ["o", "o", "x", "x", "x", "6", "o", "8", "o"]
      isAlignCross b 0 `shouldBe` False

    it "should be False" $ do
      let b = ["o", "x", "o", "x", "o", "x", "7", "o", "9"]
      isAlignCross b 2 `shouldBe` False
