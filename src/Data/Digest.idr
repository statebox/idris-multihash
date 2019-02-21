
module Data.Digest

data HashFunction = HashFn Int

Digest : Type
Digest = List Bits8

record Multihash where
  constructor MkMultihash
  hashFn : HashFunction
  hashLength : Int
  digest : Digest 

length : Digest -> Int
length d = (toIntNat $ List.length d) * 2

encode : HashFunction -> Digest -> Multihash
encode h d = MkMultihash h (Digest.length d) d

