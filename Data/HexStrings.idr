
module HexStrings

import Data.Multihash
import Data.Bytes
import Data.ByteArray

charToHex : Char -> Maybe Bits8
charToHex '0' = Just 0x0
charToHex '1' = Just 0x1
charToHex '2' = Just 0x2
charToHex '3' = Just 0x3
charToHex '4' = Just 0x4
charToHex '5' = Just 0x5
charToHex '6' = Just 0x6
charToHex '7' = Just 0x7
charToHex '8' = Just 0x8
charToHex '9' = Just 0x9
charToHex 'a' = Just 0xa
charToHex 'b' = Just 0xb
charToHex 'c' = Just 0xc
charToHex 'd' = Just 0xd
charToHex 'e' = Just 0xe
charToHex 'f' = Just 0xf
charToHex 'A' = Just 0xa
charToHex 'B' = Just 0xb
charToHex 'C' = Just 0xc
charToHex 'D' = Just 0xd
charToHex 'E' = Just 0xe
charToHex 'F' = Just 0xf
charToHex _ = Nothing

toHex : (Char, Char) -> Maybe Bits8
toHex (l, r) = do left <- charToHex l
                  right <- charToHex r
                  pure $ right `prim__orB8` (left `prim__shlB8` 0x04)

partial
from4Bits : Bits8 -> Char
from4Bits 0x0 = '0'
from4Bits 0x1 = '1'
from4Bits 0x2 = '2'
from4Bits 0x3 = '3'
from4Bits 0x4 = '4'
from4Bits 0x5 = '5'
from4Bits 0x6 = '6'
from4Bits 0x7 = '7'
from4Bits 0x8 = '8'
from4Bits 0x9 = '9'
from4Bits 0xa = 'a'
from4Bits 0xb = 'b'
from4Bits 0xc = 'c'
from4Bits 0xd = 'd'
from4Bits 0xe = 'e'
from4Bits 0xf = 'f'

pairup : List a -> List (a, a)
pairup [] = []
pairup (x :: []) = []
pairup (x :: (y :: xs)) = (x, y) :: pairup xs

fromByte : Bits8 -> List Char
fromByte b = let l = b `prim__lshrB8` 0x04
                 r = b `prim__andB8` 0x0f in
                 from4Bits l :: from4Bits r :: Nil

hexStringToBytes : String -> Maybe Bytes
hexStringToBytes str = map pack $ traverse toHex $ pairup $ unpack str

bytesToString : Bytes -> String
bytesToString = pack . concatMap fromByte . Data.Bytes.unpack

public export
implementation IMultihash String where
  encode algo body = MkMultihash algo (toIntNat $ Prelude.Strings.length body `div` 2) body
  decode hash = do bytes <- maybe (Left ParseError) Right $ hexStringToBytes hash
                   map (map bytesToString) (decode bytes)

