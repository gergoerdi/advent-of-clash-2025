{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Main where

import Clash.Prelude hiding (mapAccumL, withSomeSNat)
import GHC.TypeNats (withSomeSNat)

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

solve :: forall a n k. (Show a, Ord a, KnownNat n, KnownNat k) => SNat k -> Vec (n + k) a -> Vec k a
solve k xs = ys
  where
    n = length xs

    step :: Index k -> Index (n + k) -> (Index (n + k), a)
    step i start = (maxidx, maxval)
      where
        ~(Just (maxidx, maxval)) = foldl f Nothing xs'
          where
            f s Nothing = s
            f s (Just (idx, val)) = case s of
                Nothing -> Just (idx, val)
                Just (maxidx, maxval) -> Just $ if val > maxval then (idx, val) else (maxidx, maxval)

        xs' = fmap f (imap (,) xs)
          where
            f (j, x)
                | fromIntegral j > n - fromIntegral i - 1 = Nothing
                | j < start = Nothing
                | otherwise = Just (j, x)

    ys = unfoldr k (\(i, j) -> let (j', y) = step i j in (y, (i - 1, j' + 1))) (maxBound, 0)

fromInput :: [a] -> Vec 100 a
fromInput = L.foldr (\x xs -> fst $ shiftInAt0 xs (x :> Nil)) (pure undefined)

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc
    -- let solve' = withSomeSNat batteries \k -> solve (maxSNat (SNat @100) $ snatProxy k)
    let solve' xs = case part of
            Part1 -> let result = solve (SNat @2) xs
                    in foldl (\s x -> 10 * s + x) 0 result
            Part2 -> let result = solve (SNat @12) xs
                    in foldl (\s x -> 10 * s + x) 0 result

    problems <- lines <$> readFile inFile
    s <- sum <$> for problems \prob -> do
        let result = solve' (fromInput $ fmap digitToInt prob)
        when debug $ printf "%s %d\n" prob result
        pure result
    print s
