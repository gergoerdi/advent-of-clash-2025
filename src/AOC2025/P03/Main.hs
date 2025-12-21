{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Main where

import Options.Applicative
import Data.Traversable
import Data.Char (digitToInt)

import Debug.Trace

data Options = Options
    { inFile :: FilePath
    , batteries :: Int
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

    pure Options{..}

solve :: forall a t. (Show a, Ord a, Bounded a, Traversable t, Applicative t, t ~ []) => Int -> t a -> t a
solve n xs = r
  where
    f acc x = let acc' = maybe id max acc x in (Just acc', acc')

    go :: Int -> (t a, t [a], [a])
    go 1 = let (Just r, ys) = mapAccumR f Nothing $ map (:[]) xs
           in (xs, ys, r)
    go n = let (xs', ys, _) = go (n - 1)
               xs'' = minBound : xs'
               (Just r, ys') = mapAccumR f Nothing $ zipWith (:) xs'' ys
           in (xs'', ys', r)

    (_, _, r) = go n

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc
    problems <- lines <$> readFile inFile
    s <- sum <$> for problems \prob -> do
        let result = solve batteries (map digitToInt prob)
            val = foldl (\s x -> 10 * s + x) 0 result
        pure val
    print s
