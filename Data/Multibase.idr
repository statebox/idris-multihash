
module Data.Multibase

import Data.Vect
import public Data.Multibase.Lib
import Data.Multibase.Convert

%access export


decode : String -> Either (MultibaseError Char) (l ** b ** (MultibaseDigest l b))
decode = parseSymbols . unpack

encode : String -> BaseSymbol n -> String
encode str base {n = Z} impossible
encode str base {n = (S k)} = let inBase = unaryToBase k (digits $ S k) $ stringToBase256 str in
                                  toString base inBase
  where digits : Nat -> Nat
        digits b = toNat $ the Int $ cast (log 256 / log (cast b))

reencode : MultibaseDigest l b -> String
reencode (MkMultibaseDigest base digest) = pack $ map (`index` dictionary base) digest
