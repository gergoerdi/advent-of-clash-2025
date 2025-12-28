{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE BlockArguments, RecursiveDo, RecordWildCards #-}
module AoC2025.P07.Solve where

import Clash.Prelude
import Control.Monad.State.Strict
import qualified Data.List as L
import Control.Monad (zipWithM)
import Data.Word

data RowSt a cnt = RowSt
    { buffer :: Vec 2 a
    , hitCount :: cnt
    , isFirst :: Bool
    , timelineSum :: a
    }
    deriving (Generic, NFDataX, Show)

propagateCell :: (Num a, Eq a, Num cnt) => Bool -> a -> State (RowSt a cnt) a
propagateCell splitter above = do
    let hit = above /= 0 && splitter

    ~(RowSt{ buffer = fromLeft1 :> fromLeft2 :> Nil, .. }) <- get
    let below = fromLeft1 + if hit then above else 0
    put $ RowSt
        { buffer = fromLeft2 + gate (not hit) above :> gate hit above :> Nil
        , hitCount = hitCount + gate hit 1
        , isFirst = False
        , timelineSum = timelineSum + gate (not isFirst) below
        }
    pure below
  where
    gate b x = if b then x else 0

step :: (KnownNat n, 1 <= n, Num a, Eq a) => Bool -> Vec n a -> State (RowSt a Word32) (Vec n a)
step splitter aboves = do
    rec let (aboves', above :> Nil) = shiftInAtN aboves (below :> Nil)
        below <- propagateCell splitter above
    pure aboves'

newRow :: (Num a) => RowSt a cnt -> RowSt a cnt
newRow st = st
    { buffer = repeat 0
    , timelineSum = 0
    , isFirst = True
    }

initRow :: (Num a, Num cnt) => RowSt a cnt
initRow = RowSt
    { buffer = repeat 0
    , hitCount = 0
    , timelineSum = 0
    , isFirst = True
    }

propagateRow :: (Num a, Eq a, Num cnt) => [Bool] -> [a] -> (cnt, [a])
propagateRow row aboves = evalState `flip` initRow $ do
    belows <- zipWithM propagateCell row aboves
    fromLeft1 <- gets $ head . buffer
    hits <- gets hitCount
    pure (hits, L.tail $ belows <> [fromLeft1])
