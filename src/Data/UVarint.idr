module Data.UVarint

import Data.Bytes
import Data.ByteArray

%default total

export
data UVarintError = TooLong | UnexpectedEnd

||| 1000_0000
leadingBit : Bits8
leadingBit = the Bits8 0x80

||| Check the first bit, return True if its 1, false otherwise
continue : Bits8 -> Bool
continue b = (b `prim__andB8` leadingBit) == leadingBit

||| Accumulate all Bits8 that are part of the UVarint and return the list
||| Along with the rest of the bytes that are not part of the UVarint and
||| were not consumed
||| We are using `assert_total` because the view does not allow us to tell
||| that the recursive call is on a smaller argument on `x`
parseAcc : Bytes -> List Byte -> Either UVarintError (List Byte, Bytes)
parseAcc x acc with (consView x)
  parseAcc x acc | [] = Left UnexpectedEnd
  parseAcc x acc | (Cons b bs) = if continue b
                                    then assert_total $ parseAcc bs (b :: acc)
                                    else Right (reverse (b :: acc), bs)

||| Converts Bits8 to Bits64 by padding the left with zeroes
zeroPad : Bits8 -> Bits64
zeroPad = prim__zextB8_B64

||| Convert Nat to Int64 representation, might fail if out of bounds
partial
toInt64 : Nat -> Bits64
toInt64 = prim__zextInt_B64 . toIntNat

||| Given a bits8 and an accumualtor as Bits64, place the 7 leftmost bits of the Bits8
||| at the nth * 7 index from the right
||| ex: addAtIndex 1101_1101 acc 2 will place 101_1101 between indices 21 and 14 from
||| the right of `acc`
addAtIndex : Bits8 -> Bits64 -> Nat -> Bits64
addAtIndex b bytes index = let masked = b `prim__andB8` 0x7f 
                               offset = zeroPad masked `prim__shlB64` (toInt64 $ 7 * index) in
                               bytes `prim__orB64` offset

||| Takes a list of bits8 and concatenate all of them by removing the leading bit
||| effectively concatenating 7 bits out of 8 and padding the missing left bits
||| with zeroes
||| This fails with `TooLong` if the length of the list is greater than 9 since
||| since this would mean an integer value of more than 64 bits
||| example:      [ 1000_1100, 1110_0100, 0000_0001]
||| remove first: [  000_1100,  110_0100,  000_0001]
||| concatenate:  0_0011_0011_0010_0000_0001
||| pad:          0000_0011_0011_0010_0000_0001
concat7Last : List Bits8 -> Either UVarintError Bits64
concat7Last ls = if length ls > 9 then Left TooLong
                                  else Right . fst $ foldl accumulate (0, 0) ls
  where
    accumulate : (Bits64, Nat) -> Bits8 -> (Bits64, Nat)
    accumulate (byteAcc, index) byte = (addAtIndex byte byteAcc index, S index)

||| Parse a UVarint, maybe this should be a `Parser Int` ?
export
parseUVarint : Bytes -> Either UVarintError (Int, Bytes)
parseUVarint x = do (bytes, leftover) <- parseAcc x []
                    buffer <- concat7Last bytes
                    pure (fromInteger $ prim__zextB64_BigInt buffer, leftover)
