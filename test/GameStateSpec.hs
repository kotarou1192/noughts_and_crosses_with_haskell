module GameStateSpec where

import Data.Maybe (isNothing)
import GameState
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "turn" $ do
  it "should be 3" $ do
    let Just (t, _) = startGame >>= processGame >>= processGame
    t `shouldBe` 3

  it "should be Nothing" $ do
    let result = startGame >>= processGame >>= processGame >>= processGame >>= processGame >>= processGame >>= processGame >>= processGame >>= processGame >>= processGame >>= processGame
    isNothing result `shouldBe` True

  it "should be player 1 turn" $ do
    let Just (_, p) = startGame >>= processGame >>= processGame
    isP1Turn p `shouldBe` True

  it "should be player 2 turn" $ do
    let Just (_, p) = startGame >>= processGame >>= processGame >>= processGame
    isP2Turn p `shouldBe` True
