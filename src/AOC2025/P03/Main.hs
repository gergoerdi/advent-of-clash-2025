{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments, LambdaCase #-}
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

solve :: forall a t. (Show a, Ord a, Traversable t, Applicative t, t ~ []) => Int -> t a -> t a
solve n xs = r
  where
    (_, _, r) = go n

    scan = mapAccumR (\acc x -> let acc' = acc `max` x in (acc', acc')) Nothing

    go :: Int -> (t (Maybe a), t (Maybe (t a)), t a)
    go = \case
      1 -> (xs', ys, r)
        where
          xs' = map Just xs
          (Just r, ys) = scan $ map ((:[]) <$>) xs'
      n -> (xs'', ys', r)
        where
          (xs', ys, _) = go (n - 1)
          xs'' = Nothing : xs'
          (Just r, ys') = scan $ zipWith (liftA2 (:)) xs'' ys

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc
    problems <- lines <$> readFile inFile
    s <- sum <$> for problems \prob -> do
        let result = solve batteries (map digitToInt prob)
            val = foldl (\s x -> 10 * s + x) 0 result
        pure val
    print s
