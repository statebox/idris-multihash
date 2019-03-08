
module Test.Multihash

import Data.Multihash
import Data.HexStrings
import Data.Bytes
import Data.ByteArray
import Specdris.Spec

export
tests : IO ()
tests = spec $ do
  describe "Failing parsing tests" $ do
    it "should fail with parse error" $
      decode empty `shouldBe` Left ParseError
    it "should fail on unknown hash code" $
      decode (empty |> 0x88 |> 0x00) `shouldBe` Left CodeNotFound
  describe "Successful parsing" $ do
    it "should work on zero length" $
      decode (empty |> 0x11 |> 0x00) `shouldBe` Right (MkMultihash SHA1 0 empty)
    it "should work on non-zero length" $
      decode (empty |> 0x11 |> 0x01 |> 0xff) `shouldBe` Right (MkMultihash SHA1 1 (empty |> 0xff))
    it "should parse real sha" $
      let body = empty |> 0xcf |> 0x83 |> 0xe1 |> 0x35 |> 0x7e |> 0xef |> 0xb8 |> 0xbd |> 0xf1 |> 0x54 in
      decode ((empty |> 0x13 |> 0x0a) ++ body) `shouldBe` Right (MkMultihash SHA512 10 body)
  describe "Parsin Strings" $ do
    it "should parse empty digest" $
      decode "1100" `shouldBe` Right (MkMultihash SHA1 0 "")
    it "should parse non-empty digest" $
      decode "130acf83e1357eefb8bdf154" `shouldBe` 
        Right (MkMultihash SHA512 10 "cf83e1357eefb8bdf154")
