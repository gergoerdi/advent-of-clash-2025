{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE BlockArguments, RecursiveDo #-}
module AoC2025.P07.Solve where

import Clash.Prelude
import Control.Monad.State.Strict
import qualified Data.List as L
import Control.Monad (zipWithM)
import Data.Word

propagateCell :: (Num a, Eq a, Num cnt) => Bool -> a -> State (a, a, cnt) a
propagateCell splitter above = do
    (fromLeft1, fromLeft2, cnt) <- get
    let hit = above /= 0 && splitter
    put (fromLeft2 + gate (not hit) above, gate hit above, cnt + gate hit 1)
    let below = fromLeft1 + if hit then above else 0
    pure below
  where
    gate b x = if b then x else 0

step :: (KnownNat n, 1 <= n, Num a, Eq a) => Bool -> Vec n a -> State (a, a, Word32) (Vec n a)
step splitter aboves = do
    rec let (aboves', above :> Nil) = shiftInAtN aboves (below :> Nil)
        below <- propagateCell splitter above
    pure aboves'

propagateRow :: (Num a, Eq a, Num cnt) => [Bool] -> [a] -> (cnt, [a])
propagateRow row aboves = evalState `flip` (0, 0, 0) $ do
    belows <- zipWithM propagateCell row aboves
    (fromLeft1, _, hits) <- get
    pure (hits, L.tail $ belows <> [fromLeft1])
