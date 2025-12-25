module AoC2025.P03.BCD where

import Clash.Prelude

type Digit = Index 10
type BCD n = Vec n Digit

addBCD :: forall k n. (KnownNat n, KnownNat k, k <= n) => BCD n -> BCD k -> (Bit, BCD n)
addBCD xs ys = mapAccumR addDigit 0 $ zip xs $ leToPlus @k @n (repeat 0 ++ ys)
  where
    addDigit :: Bit -> (Digit, Digit) -> (Bit, Digit)
    addDigit cin (x, y) = (cout, z)
      where
        z0 = resize (add x y) + (fromIntegral cin :: Index 20)
        (cout, z) = if z0 > 9 then (1, resize (z0 - 10)) else (0, resize z0)
