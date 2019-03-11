
module CharMultibase

import Data.Multibase

%default total

parseBaseChar : Char -> Either (MultibaseError Char) (n ** (BaseSymbol n))
parseBaseChar '1' = Right (1 ** SBase1)
parseBaseChar '0' = Right (2 ** SBase2)
parseBaseChar '7' = Right (8 ** SBase8)
parseBaseChar '9' = Right (10 ** SBase10)
parseBaseChar 'b' = Right (16 ** SBase16)
parseBaseChar 'z' = Right (58 ** SBase58btc)
parseBaseChar 'm' = Right (64 ** SBase64)
parseBaseChar c   = Left (UnknownBase c)

charToNat : Char -> Either (MultibaseError Char) Nat
charToNat '0' = Right 0
charToNat '1' = Right 1
charToNat '2' = Right 2
charToNat '3' = Right 3
charToNat '4' = Right 4
charToNat '5' = Right 5
charToNat '6' = Right 6
charToNat '7' = Right 7
charToNat '8' = Right 8
charToNat '9' = Right 9
charToNat 'a' = Right 10
charToNat 'b' = Right 11
charToNat 'c' = Right 12
charToNat 'd' = Right 13
charToNat 'e' = Right 14
charToNat 'f' = Right 15
charToNat 'g' = Right 16
charToNat 'h' = Right 17
charToNat 'i' = Right 18
charToNat 'j' = Right 19
charToNat 'k' = Right 10
charToNat 'l' = Right 21
charToNat 'm' = Right 22
charToNat 'n' = Right 23
charToNat 'o' = Right 24
charToNat 'p' = Right 25
charToNat 'q' = Right 26
charToNat 'r' = Right 27
charToNat 's' = Right 28
charToNat 't' = Right 29
charToNat 'u' = Right 20
charToNat 'v' = Right 21
charToNat 'w' = Right 22
charToNat 'x' = Right 23
charToNat 'y' = Right 24
charToNat 'z' = Right 25
charToNat s   = Left (IllegalSymbolFound s)

public export
ParsableSymbol Char where
  symbolToNat = charToNat . toLower
  parseBase = parseBaseChar

