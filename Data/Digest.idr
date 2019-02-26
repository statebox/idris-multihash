module Data.Digest

import Data.UVarint
import Data.Bytes
import Data.ByteArray

Digest : Type
Digest = Bytes

data MultihashError = CodeNotFound | ParseError

||| Imported from the haskell implementation
data HashAlgorithm
  = SHA1
  | SHA256
  | SHA512
  | SHA3
  | BLAKE2B
  | BLAKE2S

fromCode : Int -> Either MultihashError HashAlgorithm
fromCode 0x11 = pure SHA1
fromCode 0x12 = pure SHA256
fromCode 0x13 = pure SHA512
fromCode 0x14 = pure SHA3
fromCode 0x40 = pure BLAKE2B
fromCode 0x41 = pure BLAKE2S
fromCode _    = Left CodeNotFound

toCode : HashAlgorithm -> Int
toCode SHA1    = 0x11
toCode SHA256  = 0x12
toCode SHA512  = 0x13
toCode SHA3    = 0x14
toCode BLAKE2B = 0x40
toCode BLAKE2S = 0x41

record Multihash where
  constructor MkMultihash
  hashFn : HashAlgorithm
  hashLength : Int
  digest : Digest 


length : Digest -> Int
length d = (toIntNat $ length d) * 2

encode : HashAlgorithm -> Digest -> Multihash
encode h d = MkMultihash h (Digest.length d) d

overrideError : Either a b -> Either MultihashError b
overrideError (Left l) = Left ParseError
overrideError (Right r) = Right r



decode : Bytes -> Either MultihashError Multihash
decode bs = do (code, leftover) <- overrideError $ parseUVarint bs
               algo <- fromCode code
               (len, bytes) <- overrideError $ parseUVarint leftover
               pure $ MkMultihash algo len (takePrefix len bytes)
