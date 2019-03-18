
module Data.Multibase

import Data.Vect
import public Data.Multibase.Lib
import Data.Multibase.Convert

%access export
%default total

baseLength : Nat -> Nat
baseLength b = toNat $ the Int $ cast (log 256 / log (cast b))

decode : String -> Either (MultibaseError Char) (b ** (MultibaseDigest b))
decode = parseSymbols . unpack


encode : String -> BaseSymbol n -> String
encode str base {n = Z} impossible
encode str base {n = (S k)} = let inBase = unaryToBase k (baseLength $ S k) $ stringToBase256 str in
                                  toString base inBase
    

reencode : MultibaseDigest b -> String
reencode (MkMultibaseDigest base digest) = pack $ map (`index` dictionary base) digest

groupBy : Nat -> List a -> List (List a)
groupBy len xs = if length xs < len then [xs] else let (head, tail) = splitAt len xs in assert_total $ head :: groupBy len tail

fromBaseToNat : List (Fin b) -> Nat
fromBaseToNat xs {b} = snd $ foldl (\(index, sum), val => (S index, val * (b `power` index) + sum)) (Z, Z) $ map finToNat $ reverse xs


decodeFromBase : List (Fin b) -> String
decodeFromBase xs {b} = let digitLength = baseLength b 
                            digitList = groupBy digitLength xs 
                            natList = map fromBaseToNat digitList in
                            pack $ map (chr . toIntNat) natList
                           

decodeStr : String -> Either (MultibaseError Char) String
decodeStr str = case decode str of
                     Left err => Left err
                     Right (x ** d) => Right $ decodeFromBase (digest d)
