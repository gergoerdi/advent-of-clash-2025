module AoC2025.P03.BCD where

import Clash.Prelude

type Digit = Index 10
type BCD n = Vec n Digit

addBCD :: (KnownNat n, KnownNat k) => BCD (k + n) -> BCD n -> (Bit, BCD (k + n))
addBCD xs ys = mapAccumR addDigit 0 $ zip xs (repeat 0 ++ ys)
  where
    addDigit cin (x, y) = (cout, z)
      where
        z0 = resize (add x y) + (fromIntegral cin :: Index 20)
        (cout, z) = if z0 > 9 then (1, resize (z0 - 10)) else (0, resize z0)
