{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
module Main where

import Clash.Prelude hiding (mapAccumL, withSomeSNat)

import Options.Applicative
import Data.Traversable
import qualified Data.List as L
import Data.Char (digitToInt, intToDigit)
import Text.Printf
import Control.Monad (when, guard)

data Part = Part1 | Part2

data Options = Options
    { inFile :: FilePath
    , part :: Part
    , debug :: Bool
    }

opts :: Parser Options
opts = do
    inFile <- strOption . mconcat $
      [ long "input"
      , short 'i'
      , help "input filename"
      ]

    part <- asum
      [ flag' Part1 . mconcat $
        [ long "part1"
        ]
      , flag' Part2 . mconcat $
        [ long "part2"
        ]
      ]

    debug <- switch . mconcat $
      [ long "debug"
      ]

    pure Options{..}

solve :: forall a n. forall k -> (Ord a, KnownNat n, KnownNat k) => Vec (n + k) a -> Vec k a
solve k xs = unfoldrI (\(i, j) -> let (y, j') = findMax xs i j in (y, (i - 1, j' + 1))) (maxBound :: Index k, 0)

findMax :: forall a n k. (Ord a, KnownNat n, KnownNat k) => Vec (n + k) a -> Index k -> Index (n + k) -> (a, Index (n + k))
findMax xs i start = (maxval, maxidx)
  where
    ~(Just (maxidx, maxval)) = foldl f Nothing xs'
      where
        f s Nothing = s
        f s (Just (idx, val)) = case s of
            Nothing -> Just (idx, val)
            Just (maxidx, maxval) -> Just $ if val > maxval then (idx, val) else (maxidx, maxval)

    xs' = fmap f (imap (,) xs)
      where
        f :: (Index (n + k), a) -> Maybe (Index (n + k), a)
        f (j, x) = (j, x) <$ guard (inside j)

    end :: Index (n + k)
    end = snatToNum (SNat @n) + fromIntegral (maxBound - i)

    inside j = j >= fromIntegral start && j <= end


fromInput :: [a] -> Vec 100 a
fromInput = L.foldr (\x xs -> fst $ shiftInAt0 xs (x :> Nil)) (pure undefined)

toOutput :: (Num a) => Vec k a -> a
toOutput = foldl (\s x -> 10 * s + x) 0

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc
    let solve' = case part of
            Part1 -> toOutput . solve 2
            Part2 -> toOutput . solve 12

    problems <- lines <$> readFile inFile
    s <- sum <$> for problems \prob -> do
        let result = solve' (fromInput $ fmap digitToInt prob)
        when debug $ printf "%s %d\n" prob result
        pure result
    print s
