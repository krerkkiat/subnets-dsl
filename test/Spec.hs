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

  describe "Subnet.canGrowBy" $ do
    it "can grow by 100 percent" $ do
      SubnetSlash "172.18.128.0/20" 2000 "Wireless" `shouldSatisfy` canGrowBy 1.0
    it "can grow by 50 percent" $ do
      SubnetSlash "172.18.128.0/24" 127 "Sale" `shouldSatisfy` canGrowBy 0.5
    it "cannot grow by 100 percent" $ do
      SubnetSlash "172.18.128.0/24" 127 "Sale" `shouldNotSatisfy` canGrowBy 1.0
