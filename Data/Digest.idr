module Data.Digest

import Data.UVarint
import Data.Bytes
import Data.ByteArray

public export
Digest : Type
Digest = Bytes

%access export

public export
data MultihashError = CodeNotFound | ParseError

Show MultihashError where
  show CodeNotFound = "Unknown code for hash algorithm"
  show ParseError = "Parse Error"

Eq MultihashError where
  CodeNotFound == CodeNotFound = True
  ParseError == ParseError = True
  _ == _ = False

||| Imported from the haskell implementation
public export
data HashAlgorithm
  = SHA1
  | SHA256
  | SHA512
  | SHA3
  | BLAKE2B
  | BLAKE2S

export
Show HashAlgorithm where
  show SHA1    = "SHA1"
  show SHA256  = "SHA256"
  show SHA512  = "SHA512"
  show SHA3    = "SHA3"
  show BLAKE2B = "BLAKE2B"
  show BLAKE2S = "BLAKE2S"


Eq HashAlgorithm where
  SHA1 == SHA1 = True
  SHA256 == SHA256 = True
  SHA512 == SHA512 = True
  SHA3 == SHA3 = True
  BLAKE2B == BLAKE2B = True
  BLAKE2S == BLAKE2S = True
  _ == _ = False

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

export
record Multihash where
  constructor MkMultihash
  hashFn : HashAlgorithm
  hashLength : Int
  digest : Digest 

export
Show Multihash where
  show (MkMultihash hashFn hashLength digest) = "Multihash(" ++ show hashFn 
    ++ ", " ++ show hashLength
    ++ ", " ++ show digest
    ++ ")"

Eq Multihash where
  (MkMultihash hashFnl hashLengthl digestl) == (MkMultihash hashFnr hashLengthr digestr) = 
    hashFnl == hashFnr &&
    hashLengthl == hashLengthr &&
    digestl == digestr

private
length : Digest -> Int
length d = (toIntNat $ length d) * 2

encode : HashAlgorithm -> Digest -> Multihash
encode h d = MkMultihash h (Digest.length d) d

overrideError : Either a b -> Either MultihashError b
overrideError (Left l) = Left ParseError
overrideError (Right r) = Right r

export
decode : Bytes -> Either MultihashError Multihash
decode bs = do (code, leftover) <- overrideError $ parseUVarint bs
               algo <- fromCode code
               (len, bytes) <- overrideError $ parseUVarint leftover
               pure $ MkMultihash algo len (takePrefix len bytes)
