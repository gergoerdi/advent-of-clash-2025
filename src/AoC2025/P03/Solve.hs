{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE BlockArguments #-}
module AoC2025.P03.Solve where

import Clash.Prelude

import Data.Traversable
import Data.Foldable (maximumBy)
import Data.Ord (Down(..), comparing)

solve :: forall a n. forall k -> (Ord a, KnownNat n, KnownNat k, 1 <= k, 1 <= n) => Vec (n + k) a -> Vec k a
solve k xs = unfoldrI f (0, fromSNat (SNat @n))
  where
    f (start, end) = (x, (i + 1, end + 1))
      where
        (i, x) = findMaxBetween start end xs

findMaxBetween :: forall n a. (Ord a, KnownNat n, 1 <= n) => Index n -> Index n -> Vec n a -> (Index n, a)
findMaxBetween start end =
    unscore .
    leToPlus @1 @n head .
    iterateN (maxBound :: Index (CLog 2 n + 1)) (rollup max) .
    imap (score start end)
  where
    iterateN :: forall k a. (KnownNat k) => Index k -> (a -> a) -> a -> a
    iterateN k f x = if k == 0 then x else iterateN (k - 1) f (f x)

score :: (KnownNat n) => Index n -> Index n -> Index n -> a -> (Bool, a, Down (Index n))
score start end = \i x -> (inside i, x, Down i)
  where
    inside i = i >= start && i <= end

unscore :: (Bool, a, Down (Index n)) -> (Index n, a)
unscore (_, x, Down i) = (i, x)

rollup :: forall n a. (KnownNat n, 1 <= n) => (a -> a -> a) -> Vec n a -> Vec n a
rollup f xs = map pos indicesI
  where
    pos i
        | i < snatToNum (SNat @(n `Div` 2)) = f (xs !! i') (xs !! (i' + 1))
        | otherwise = leToPlus @1 @n last xs
      where
        i' = i `shiftL` 1
