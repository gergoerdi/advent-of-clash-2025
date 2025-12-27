{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE BlockArguments, RecursiveDo #-}
module AoC2025.P07.Solve where

import Clash.Prelude
import Control.Monad.State.Strict
import qualified Data.List as L
import Control.Monad (zipWithM)

propagateCell :: (Num a, Eq a) => Bool -> a -> State (a, a) (Bool, a)
propagateCell splitter above = do
    (fromLeft1, fromLeft2) <- get
    let hit = above /= 0 && splitter
    put (fromLeft2 + if not hit then above else 0, if hit then above else 0)
    let below = fromLeft1 + if hit then above else 0
    pure (hit, below)

propagateRow :: (Num a, Eq a) => [Bool] -> [a] -> (Int, [a])
propagateRow row aboves = evalState `flip` (0, 0) $ do
    (hits, belows) <- L.unzip <$> zipWithM propagateCell row aboves
    (fromLeft1, _) <- get
    pure (count hits, L.tail $ belows <> [fromLeft1])
  where
    count :: [Bool] -> Int
    count = L.length . filter id
