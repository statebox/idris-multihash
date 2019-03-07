
module HexStrings

import Data.Multihash
import Data.Bytes


toHex : Char -> Maybe Bits8
toHex '0' = Just 0x0
toHex '1' = Just 0x1
toHex '2' = Just 0x2
toHex '3' = Just 0x3
toHex '4' = Just 0x4
toHex '5' = Just 0x5
toHex '6' = Just 0x6
toHex '7' = Just 0x7
toHex '8' = Just 0x8
toHex '9' = Just 0x9
toHex 'a' = Just 0xa
toHex 'b' = Just 0xb
toHex 'c' = Just 0xc
toHex 'd' = Just 0xd
toHex 'e' = Just 0xe
toHex 'f' = Just 0xf
toHex 'A' = Just 0xa
toHex 'B' = Just 0xb
toHex 'C' = Just 0xc
toHex 'D' = Just 0xd
toHex 'E' = Just 0xe
toHex 'F' = Just 0xf
toHex _ = Nothing

fromByte : Bits8 -> Char
fromByte 0x0 = '0'
fromByte 0x1 = '1'
fromByte 0x2 = '2'
fromByte 0x3 = '3'
fromByte 0x4 = '4'
fromByte 0x5 = '5'
fromByte 0x6 = '6'
fromByte 0x7 = '7'
fromByte 0x8 = '8'
fromByte 0x9 = '9'
fromByte 0xa = 'a'
fromByte 0xb = 'b'
fromByte 0xc = 'c'
fromByte 0xd = 'd'
fromByte 0xe = 'e'
fromByte 0xf = 'f'


hexStringToBytes : String -> Maybe Bytes
hexStringToBytes str = map pack $ traverse toHex $ unpack str

bytesToString : Bytes -> String
bytesToString = pack . map fromByte . Data.Bytes.unpack

implementation IMultihash String where
  encode algo body = MkMultihash algo (toIntNat $ Prelude.Strings.length body `div` 2) body
  decode hash = do bytes <- maybe (Left ParseError) Right $ hexStringToBytes hash
                   map (map bytesToString) (decode @{multihashBytes} bytes)

