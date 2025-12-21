{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Main where

import Options.Applicative
import Data.Traversable
import Data.Char (digitToInt)
import Text.Printf
import Control.Monad (when)

import Debug.Trace

data Options = Options
    { inFile :: FilePath
    , batteries :: Int
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

solve :: forall a t. (Ord a, Traversable t, Applicative t, t ~ []) => a -> Int -> t a -> t a
solve lim n xs = r
  where
    (_, _, r) = go n

    scan = mapAccumR (\acc x -> let acc' = acc `max` x in (acc', acc')) (pure lim)

    go = \case
        1 -> (xs, ys, r)
          where
            (r, ys) = scan $ map (:[]) xs
        n -> (xs'', ys', r)
          where
            (xs', ys, _) = go (n - 1)
            xs'' = lim : xs'
            (r, ys') = scan $ zipWith (:) xs'' ys

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc
    problems <- lines <$> readFile inFile
    s <- sum <$> for problems \prob -> do
        let result = solve 0 batteries (map digitToInt prob)
            val = foldl (\s x -> 10 * s + x) 0 result
        when debug $ printf "%s %d\n" prob val
        pure val
    print s
