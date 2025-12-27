{-# LANGUAGE RecordWildCards, BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
module AoC2025.P03.Main where

import Clash.Prelude

import AoC2025.CLI
import AoC2025.P03.Solve
import AoC2025.P03.BCD

import qualified Data.List as L
import Data.Char (digitToInt)
import Text.Printf
import Control.Monad (when, foldM)

type LineLen = 100

fromInput :: [a] -> Vec LineLen a
fromInput = L.foldr (\x xs -> fst $ shiftInAt0 xs (x :> Nil)) (pure undefined)

fromBCD :: (KnownNat n) => BCD n -> Natural
fromBCD = foldl (\s d -> 10 * s + fromIntegral d) 0

main :: IO ()
main = do
    Options{..} <- getOptions

    let solveLine :: BCD 20 -> String -> IO (BCD 20)
        solveLine = case part of
            Part1 -> f (solve 2)
            Part2 -> f (solve 12)
          where
            f :: forall k k0. (KnownNat k, k <= 20) => (BCD LineLen -> BCD k) -> BCD 20 -> String -> IO (BCD 20)
            f solve' s prob = do
                let result = solve' $ fromInput . fmap (fromIntegral . digitToInt) $ prob
                when verbose $ printf "%s %d\n" prob (fromBCD result)
                pure $ addBCD s result

    problems <- lines <$> readFile inFile
    s <- foldM solveLine (repeat 0) problems

    print $ fromBCD s
