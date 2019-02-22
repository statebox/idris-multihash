module Data.UVarint

import Data.Bytes
import Data.ByteArray

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
parseAcc : Bytes -> List Bits8 -> Either UVarintError (List Bits8, Bytes)
parseAcc x acc with (consView x)
  parseAcc x acc | [] = Left UnexpectedEnd
  parseAcc x acc | (Cons b bs) = if continue b
                                    then parseAcc bs (b :: acc)
                                    else Right (reverse (b :: acc), bs)

||| Parse a UVarint, maybe this should be a `Parser Int` ?
parseUVarint : Bytes -> Either UVarintError (List Bits8, Bytes)
parseUVarint x = parseAcc x []


||| Takes a list of bits8 and concatenate all of them by removing the leading bit
||| effectively concatenating 7 bits out of 8 and padding the missing left bits
||| with zeroes
||| This fails with `TooLong` if the length of the list is greater than 9 since
||| since this would mean an integer value of more than 64 bits
||| example:      [ 1000_1100, 1110_0100, 0000_0001]
||| remove first: [  000_1100,  110_0100,  000_0001]
||| concatenate:  0_0011_0011_0010_0000_0001
||| pad:          0000_0011_0011_0010_0000_0001
concat7Last : List Bits8 -> Either UVarintError Int
concat7Last bits = Left TooLong

