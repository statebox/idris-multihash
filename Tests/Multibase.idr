
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
    it "should encode and decode identity" $ do
      (map (reencode . ?imeanwhut) $ decode "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe") `shouldBe` 
      Right "zUXE7GvtEk8XTXs1GF8HSGbVA9FCX9SEBPe"

      
