
module Data.Multibase.Convert

import Data.Vect
import Data.Nat.DivMod

%access export
%default total 

toFin : (n : Nat) -> Fin (S n)
toFin Z = FZ
toFin (S k) = FS (toFin k)

mkFin : {n, m : Nat} -> Fin (S (n + m))
mkFin {n = n} {m = Z} = rewrite plusZeroRightNeutral n in toFin n
mkFin {n = n} {m = (S k)} = rewrite sym $ plusSuccRightSucc n k in weaken mkFin

finSuccPlusRight : Fin (S (n + m)) -> Fin (n + (S m))
finSuccPlusRight x {n} {m} = rewrite sym $ plusSuccRightSucc n m in x

data DivNat : Nat -> Type where
  NoRem : (k : _) -> DivNat (k * n)
  SomeRem : (r, l : Nat) -> DivNat (k * n + (S r))

lteEqPlus : {a, b : Nat} -> a `LTE` b -> b = a + (b - a)
lteEqPlus LTEZero {b} = rewrite minusZeroRight b in Refl
lteEqPlus (LTESucc x) = cong $ lteEqPlus x

natLteFin : LTE (S n) (S f) -> (n : Nat) -> Fin (S f)
natLteFin lte Z {f = f} = FZ
natLteFin (LTESucc lte) (S k) {f = f} = rewrite lteEqPlus lte in mkFin

||| Convert a natural number into the selected base (actually the predecessor of the base). The output
||| is a list of natural numbers smaller than the base which acts as an upper bound. LSB
convertBaseList : (predBase : Nat) -> Nat -> List (Fin (S predBase)) -> List (Fin (S predBase))
convertBaseList predBase num acc with (num `divMod` predBase)
  convertBaseList predBase (remainder + (    Z * (S predBase))) acc | (MkDivMod     Z remainder remainderSmall) = 
    (natLteFin remainderSmall remainder) :: acc
  convertBaseList predBase (remainder + ((S q) * (S predBase))) acc | (MkDivMod (S q) remainder remainderSmall) = 
    -- This assert_total is a bit frustrating, we can see that  q is smaller than r + q * b but idris can't figure it out
    assert_total $ convertBaseList predBase (S q) ((natLteFin remainderSmall remainder) :: acc)

padWithZero : Nat -> List (Fin (S n)) -> List (Fin (S n))
padWithZero k xs with (k `minus` length xs)
  | Z = xs
  | (S n) = replicate (S n) FZ ++ xs

||| Nats represent values in unary base
unaryToBase : (predBase : Nat) -> Nat -> List Nat -> List (Fin (S predBase))
unaryToBase predBase padding xs = xs >>= (\char => padWithZero padding (convertBaseList predBase char []))

||| Convert a string into a list of Nat each representing a number between 0 and 255
stringToBase256 : String -> List Nat
stringToBase256 x = map PE_toNat_abb9c3f3 $ unpack x

||| This in effect, transforms a list of numbers in a base into a unary base (Nat)
listBaseToNat : List (Fin base) -> Nat
listBaseToNat ls = listBaseToNatHelper ls 0 0
  where
    listBaseToNatHelper : List (Fin base) -> Nat -> Nat -> Nat
    listBaseToNatHelper [] index acc = acc
    listBaseToNatHelper (y :: xs) index acc {base} = 
      listBaseToNatHelper xs (S index) (acc + ((finToNat y) * (base `power` index)))

