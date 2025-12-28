{-# OPTIONS -fconstraint-solver-iterations=10 #-}
module AoC2025.BCD where

import Clash.Prelude

type Digit = Index 10
type BCD n = Vec n Digit

toDigit :: Unsigned 4 -> Digit
toDigit = bitCoerce

addBCD :: forall k n. (KnownNat n, KnownNat k, k <= n) => BCD n -> BCD k -> BCD n
addBCD xs ys = snd $ mapAccumR addDigit 0 $ zip xs $ leToPlus @k @n (repeat 0 ++ ys)

addDigit :: Bit -> (Digit, Digit) -> (Bit, Digit)
addDigit cin (x, y) = (cout, z)
  where
    z0 = resize (add x y) + (fromIntegral cin :: Index 20)
    (cout, z) = if z0 > 9 then (1, resize (z0 - 10)) else (0, resize z0)

type BCDSize n = CLog 10 (2 ^ n)
type ShiftAdd n = (Vec (BCDSize n) (Unsigned 4), Unsigned n)

{-# INLINE initBCD #-}
initBCD :: (KnownNat n) => Unsigned n -> ShiftAdd n
initBCD = (,) (repeat 0)

stepBCD :: (KnownNat n) => ShiftAdd n -> ShiftAdd n
stepBCD = shift . add
  where
    shift :: (KnownNat n) => ShiftAdd n -> ShiftAdd n
    shift = unpack . (`shiftL` 1) . pack

    add :: ShiftAdd n -> ShiftAdd n
    add (digits, buf) = (map add3 digits, buf)
      where
        add3 d = if d >= 5 then d + 3 else d

{-# INLINE finishBCD #-}
finishBCD :: (KnownNat n) => ShiftAdd n -> BCD (BCDSize n)
finishBCD = map toDigit . fst

applyN :: SNat n -> (a -> a) -> a -> a
applyN n f = last . iterate (succSNat n) f

{-# INLINE toBCD #-}
toBCD :: forall n. (KnownNat n) => Unsigned n -> BCD (BCDSize n)
toBCD = finishBCD . applyN (SNat @n) stepBCD . initBCD
