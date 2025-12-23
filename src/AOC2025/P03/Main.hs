{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Main where

import Clash.Prelude hiding (mapAccumR, mapAccumL)

import Options.Applicative
import Data.Traversable
import qualified Data.List as L
import Data.Char (digitToInt, intToDigit)
import Text.Printf
import Control.Monad (when)
import Debug.Trace

import Debug.Trace

data Options = Options
    { inFile :: FilePath
    , batteries :: Natural
    , debug :: Bool
    }

opts :: Parser Options
opts = do
    inFile <- strOption . mconcat $
      [ long "input"
      , short 'i'
      , help "input filename"
      ]

    batteries <- option auto . mconcat $
      [ long "batteries"
      , short 'n'
      , help "Number of batteries to choose"
      , value 2
      ]

    debug <- switch . mconcat $
      [ long "debug"
      ]

    pure Options{..}

solve :: forall a n k. (Show a, Ord a, KnownNat n, KnownNat k) => SNat k -> Vec (n + k) a -> Vec k a
solve k xs = ys
  where
    n = length xs

    step :: Index k -> Index (n + k) -> (a, Index (n + k))
    step i start = (acc, idx + 1)
      where
        (~(Just acc), accs) = mapAccumL f Nothing xs'
          where
            f acc x' = let acc' = max acc x' in (acc', acc')

        ~(Just idx) = elemIndex (Just acc) accs

        xs' = fmap f (imap (,) xs)
          where
            f (j, x)
                | fromIntegral j > n - fromIntegral i - 1 = Nothing
                | j < start = Nothing
                | otherwise = Just x

    ys = unfoldr k (\(i, j) -> let (y, j') = step i j in (y, (i - 1, j'))) (maxBound, 0)

fromInput :: [a] -> Vec 100 a
fromInput = L.foldr (\x xs -> fst $ shiftInAt0 xs (x :> Nil)) (pure undefined)

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc
    problems <- lines <$> readFile inFile
    s <- sum <$> for problems \prob -> do
        let result = solve (SNat @12) (fromInput $ fmap digitToInt prob)
            val = foldl (\s x -> 10 * s + x) 0 result
        when debug $ printf "%s %d\n" prob val
        pure val
    print s
