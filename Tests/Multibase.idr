
module Test.Multibase

import Data.Multibase
import Specdris.Spec
import Data.CharMultibase
import Data.Vect

tests : IO ()
tests = spec $ do 
  describe "Successful parsing tests" $ do
    it "should decode base 1" $ do
      decode "1000000" `shouldBe` 
      Right (6 ** 1 ** MkMultibaseDigest SBase1 [0, 0, 0, 0, 0, 0])
--     it "should encode and decode identity" $ do
--       (map (reencode . ?imeanwhut) $ decode "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe") `shouldBe` 
--       Right "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe"

  describe "Multibase encoding" $ do
    it "should encode in base 2" $ do
      encode "yes mani !" SBase2 `shouldBe` "01111001011001010111001100100000011011010110000101101110011010010010000000100001"
    it "should encode in base 8" $ do
      encode "yes mani !" SBase8 `shouldBe` "7171312714403326055632220041"
    it "should encode in base 10" $ do
      encode "yes mani !" SBase10 `shouldBe` "9573277761329450583662625"

      
