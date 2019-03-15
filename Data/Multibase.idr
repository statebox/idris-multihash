
module Data.Multibase

import Data.Vect
import public Data.Multibase.Lib
import Data.Multibase.Convert

%access export


decode : String -> Either (MultibaseError Char) (l ** b ** (MultibaseDigest l b))
decode = parseSymbols . unpack

encode : String -> BaseSymbol n -> String
encode str base {n} = let as256Base = stringToBase256 str 
                          inBase = as256Base >>= convertBase n in
                          ?idk

reencode : MultibaseDigest l b -> String
reencode (MkMultibaseDigest base digest) = pack $ map (`index` dictionary base) digest
