import Test.Hspec
import Data.Subnet

main :: IO ()
main = hspec $ do
  describe "Subnet.inetAtoN" $ do
    it "basic value" $ do
      inetAtoN "192.168.0.24" `shouldBe` 3232235544

  describe "Subnet.inetNtoA" $ do
    it "basic value" $ do
      inetNtoA 3232235544 `shouldBe` "192.168.0.24"
