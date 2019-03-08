
module Data.Multibase

import Data.Fin
import Data.Vect

%default total

%access export

public export
data BaseSymbol : Nat -> Type where
  SBase1     : BaseSymbol 0
  SBase2     : BaseSymbol 1
  SBase8     : BaseSymbol 7
  SBase10    : BaseSymbol 9
  SBase16    : BaseSymbol 15
  SBase32    : BaseSymbol 31
  SBase58btc : BaseSymbol 57
  SBase64    : BaseSymbol 63
  
public export
record MultibaseDigest (length : Nat) (n : Nat) where
  constructor MkMultibaseDigest
  base : BaseSymbol n
  digest : Vect length (Fin n)


public export
data MultibaseError = UnknownBase | IllegalSymbolFound | DigestEmpty

||| Interface for values which can be converted to Nat
||| The Nat representation is used to parse the symbol as a digit in 
||| any arbitrary base
public export interface ParsableSymbol a where
   symbolToNat : a -> Nat
   parseBase : a -> Either MultibaseError (n ** BaseSymbol n)

||| Given a base and a vector of nat check if any nat go out of bound of the specified base
parseDigest : (b : BaseSymbol n) -> Vect l Nat -> Either MultibaseError (MultibaseDigest l n)
parseDigest b [] {n = n} = Right (MkMultibaseDigest b [])
parseDigest b (x :: xs) {n = n} = do fin <- maybe (Left IllegalSymbolFound) Right $ natToFin x n
                                     MkMultibaseDigest b fs <- parseDigest b xs
                                     pure (MkMultibaseDigest b (fin :: fs))

||| Given a base and a vector of symbols check if all symbols are correctly encoded in the base
parse : ParsableSymbol sym => (b : BaseSymbol n) -> Vect l sym -> Either MultibaseError (MultibaseDigest l n)
parse b digest = let symbols = map symbolToNat digest in
                     parseDigest b symbols

||| Given a list of parsable symbols return the digest indexed by its length and base number
parseSymbols : ParsableSymbol s => List s -> Either MultibaseError (l ** b ** (MultibaseDigest l b))
parseSymbols [] = Left DigestEmpty
parseSymbols (x :: xs) with (parseBase x) 
  | (Left y) = Left y
  | (Right (n ** base)) = let vect = fromList xs in
                              case parse base vect of
                                   Left err => Left err
                                   Right parsed => Right ((length xs) ** n ** parsed)
