{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
module AoC2025.P03.Solve where

import Clash.Prelude

import Data.Traversable
import Data.Foldable (maximumBy)
import Data.Ord (Down(..), comparing)

solve :: forall a n. forall k -> (Ord a, KnownNat n, KnownNat k) => Vec (n + k) a -> Vec k a
solve k xs = unfoldrI f (0, snatToNum (SNat @n))
  where
    f (start, end) = (x, (i + 1, end + 1))
      where
        (i, x) = findMaxBetween start end xs

findMaxBetween :: (Ord a, KnownNat n) => Index n -> Index n -> Vec n a -> (Index n, a)
findMaxBetween start end = maximumBy (comparing eval) . imap (,)
  where
    inside i = i >= start && i <= end
    eval (i, x) = (inside i, x, Down i)
