
module Data.Multibase

import Data.Fin
import Data.Vect

%default total

%access export

public export
data BaseSymbol : Nat -> Type where
  SBase1     : BaseSymbol 1
  SBase2     : BaseSymbol 2
  SBase8     : BaseSymbol 8
  SBase10    : BaseSymbol 10
  SBase16    : BaseSymbol 16
  SBase32    : BaseSymbol 32
  SBase58btc : BaseSymbol 58
  SBase64    : BaseSymbol 64

Show (BaseSymbol n) where
  show SBase1     = "1"
  show SBase2     = "binary"
  show SBase8     = "octal"
  show SBase10    = "decimal"
  show SBase16    = "hexadecimal"
  show SBase32    = "32bits"
  show SBase58btc = "58bits"
  show SBase64    = "64bits"

Eq (BaseSymbol n) where
  SBase1     == SBase1     = True 
  SBase2     == SBase2     = True
  SBase8     == SBase8     = True
  SBase10    == SBase10    = True
  SBase16    == SBase16    = True
  SBase32    == SBase32    = True
  SBase58btc == SBase58btc = True
  SBase64    == SBase64    = True
  _ == _ = False

||| Record that holds the digest and the meta-data about it
||| It is indexed by the length of the digest and the size of the base.
||| This representation is agnostic from the original encoding, that is
||| there could be multiple encodings that represent 64bits bases but
||| they will have the same `MultibaseDigest`
public export record MultibaseDigest (length : Nat) (n : Nat) where
  constructor MkMultibaseDigest
  base : BaseSymbol n
  digest : Vect length (Fin n)

||| This probably should be in STD-lib
implementation (Eq a, {y : a} -> Eq (p y)) => Eq (DPair a p) where
  lhs == rhs = (fst lhs == fst rhs) && (snd rhs == snd rhs)

Eq (MultibaseDigest l b) where
  (MkMultibaseDigest b1 d1) == (MkMultibaseDigest b2 d2) = b1 == b2 && d1 == d2

Show (Fin b) where
  show = show . finToInteger

Show (MultibaseDigest l b) where
  show (MkMultibaseDigest base digest) = show base ++ ":" ++ show digest


public export
data MultibaseError char = UnknownBase char 
                         | IllegalSymbolFound char 
                         | DigestEmpty 
                         | OutOfRangeSymbol Nat

Eq c => Eq (MultibaseError c) where
  (UnknownBase c)        == (UnknownBase c')        = c == c'
  (IllegalSymbolFound s) == (IllegalSymbolFound s') = s == s'
  DigestEmpty            == DigestEmpty             = True 
  (OutOfRangeSymbol n)   == (OutOfRangeSymbol n')   = n == n'
  _ == _ = False

Show c => Show (MultibaseError c) where
  show (UnknownBase c) = "UnknownBase: " ++ show c
  show (IllegalSymbolFound s) = "IllegalSymbolFound: " ++ show s
  show DigestEmpty = "DigestEmpty"
  show (OutOfRangeSymbol n) = "OutOfRangeSymbol: " ++ show n

||| Interface for values which can be converted to Nat
||| The Nat representation is used to parse the symbol as a digit in 
||| any arbitrary base
public export interface ParsableSymbol a where
   symbolToNat : a -> Either (MultibaseError a) Nat
   parseBase : a -> Either (MultibaseError a) (n ** BaseSymbol n)

||| Given a base and a vector of nat check if any nat go out of bound of the specified base
parseDigest : (b : BaseSymbol n) -> Vect l Nat -> Either (MultibaseError a) (MultibaseDigest l n)
parseDigest b [] {n = n} = Right (MkMultibaseDigest b [])
parseDigest b (x :: xs) {n = n} = do fin <- maybe (Left (OutOfRangeSymbol x)) Right $ natToFin x n
                                     MkMultibaseDigest b fs <- parseDigest b xs
                                     pure (MkMultibaseDigest b (fin :: fs))

||| Given a base and a vector of symbols check if all symbols are correctly encoded in the base
parse : ParsableSymbol sym => (b : BaseSymbol n) -> Vect l sym -> Either (MultibaseError sym) (MultibaseDigest l n)
parse b digest = do symbols <- traverse symbolToNat digest
                    parseDigest b symbols

||| Given a list of parsable symbols return the digest indexed by its length and base number
parseSymbols : ParsableSymbol s => List s -> Either (MultibaseError s) (l ** b ** (MultibaseDigest l b))
parseSymbols [] = Left DigestEmpty
parseSymbols (x :: xs) with (parseBase x) 
  | (Left y) = Left y
  | (Right (n ** base)) = let vect = fromList xs in
                              case parse base vect of
                                   Left err => Left err
                                   Right parsed => Right ((length xs) ** n ** parsed)
