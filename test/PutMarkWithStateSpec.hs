module PutMarkWithStateSpec where

import PutMarkWithState
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "board" $ do
  it "process game" $ do
    let Just (m1, m2, gs, b) = startGameWithPlayer 3 "x" "o" >>= playGame 1 >>= playGame 3 >>= playGame 5
    b `shouldBe` ["x", "2", "o", "4", "x", "6", "7", "8", "9"]
