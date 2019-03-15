module Data.Multibase.Lib

import Data.Fin
import Data.Vect

%access export
%default total

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

dictionary : BaseSymbol b -> Vect b Char
dictionary SBase1 = fromList $ unpack "0"
dictionary SBase2 = fromList $ unpack "01"
dictionary SBase8 = fromList $ unpack "01234567"
dictionary SBase10 = fromList $ unpack "0123456789"
dictionary SBase16 = fromList $ unpack "0123456789abcdef"
dictionary SBase32 = fromList $ unpack "0123456789abcdefghijklmnopqrstuv"
dictionary SBase58btc = fromList $ unpack "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
dictionary SBase64 = fromList $ unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

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
implementation (DecEq a, {y : a} -> Eq (p y)) => Eq (DPair a p) where
   (x ** pf) == (y ** pf') with (decEq x y)
     (x ** pf) == (x ** pf') | Yes Refl = pf == pf'
     (x ** pf) == (y ** pf') | No contra = False



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
   symbolToNat : BaseSymbol b -> a -> Either (MultibaseError a) Nat
   parseBase : a -> Either (MultibaseError a) (n ** BaseSymbol n)

||| Given a base and a vector of nat check if any nat go out of bound of the specified base
parseDigest : (b : BaseSymbol n) -> Vect l Nat -> Either (MultibaseError a) (MultibaseDigest l n)
parseDigest b [] {n = n} = Right (MkMultibaseDigest b [])
parseDigest b (x :: xs) {n = n} = do fin <- maybe (Left (OutOfRangeSymbol x)) Right $ natToFin x n
                                     MkMultibaseDigest b fs <- parseDigest b xs
                                     pure (MkMultibaseDigest b (fin :: fs))

||| Given a base and a vector of symbols check if all symbols are correctly encoded in the base
parse : ParsableSymbol sym => (b : BaseSymbol n) -> Vect l sym -> Either (MultibaseError sym) (MultibaseDigest l n)
parse b digest = do symbols <- traverse (symbolToNat b) digest
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

parseBaseChar : Char -> Either (MultibaseError Char) (n ** (BaseSymbol n))
parseBaseChar '1' = Right (1 ** SBase1)
parseBaseChar '0' = Right (2 ** SBase2)
parseBaseChar '7' = Right (8 ** SBase8)
parseBaseChar '9' = Right (10 ** SBase10)
parseBaseChar 'b' = Right (16 ** SBase16)
parseBaseChar 'z' = Right (58 ** SBase58btc)
parseBaseChar 'm' = Right (64 ** SBase64)
parseBaseChar c   = Left (UnknownBase c)

public export ParsableSymbol Char where
  symbolToNat base char = let dict = dictionary base 
                              index = char `elemIndex` dict in 
                              maybe (Left (IllegalSymbolFound char)) (Right . finToNat) index
  parseBase = parseBaseChar


writeString : String -> List Bits8

encodeBits : List Bits8 -> BaseSymbol n -> (l ** MultibaseDigest l n)
encodeBits xs x = ?encodeBits_rhs
