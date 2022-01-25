import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "output" $ do
  it "should be" $ do
    True `shouldBe` True
