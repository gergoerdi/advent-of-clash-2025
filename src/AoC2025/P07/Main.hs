{-# LANGUAGE RecordWildCards, BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
module AoC2025.P07.Main where

import Clash.Prelude

import AoC2025.CLI
import AoC2025.P07.Solve
import Control.Monad.State.Strict

import qualified Data.List as L
import Data.Traversable (for)
import Data.Char (digitToInt)
import Text.Printf
import Control.Monad (when)

type LineLen = 141

fromStart :: forall n -> (KnownNat n) => String -> Vec n Int
fromStart n = L.foldl (\s c -> s <<+ (if c == 'S' then 1 else 0)) (repeat 0)

render :: String -> [Int] -> String
render row = L.zipWith render1 row
  where
    render1 c b = if b /= 0 then '|' else c

main :: IO ()
main = do
    Options{..} <- getOptions
    (start:rows) <- lines <$> readFile inFile

    let s0 = fmap (\c -> if c == 'S' then 1 else 0) start

    (count, paths) <- evalStateT `flip` s0 $ do
        counts <- for rows \row -> do
            count <- state $ propagateRow $ (== '^') <$> row
            when verbose do
                s <- get
                liftIO $ printf "%s %d %d\n" (render row s) count (sum s)
            pure count
        paths <- gets sum
        pure (sum counts, paths)

    case part of
        Part1 -> print @Int count
        Part2 -> print @Int paths
