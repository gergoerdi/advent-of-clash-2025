{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
module Main where

import Clash.Prelude hiding (mapAccumL, withSomeSNat)

import Options.Applicative
import Data.Traversable
import qualified Data.List as L
import Data.Char (digitToInt, intToDigit)
import Text.Printf
import Control.Monad (when)

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
solve k xs = unfoldrI f (0, snatToNum (SNat @n))
  where
    f (start, end) = (x, (i + 1, end + 1))
      where
        ~(Just (x, i)) = findMaxBetween start end xs

findMaxBetween :: forall a n k. (Ord a, KnownNat n) => Index n -> Index n -> Vec n a -> Maybe (a, Index n)
findMaxBetween start end xs = ifoldl f Nothing xs
  where
    f acc i x
        | i < start || i > end = acc
        | Just (maxx, maxi) <- acc, maxx >= x = acc
        | otherwise = Just (x, i)

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
