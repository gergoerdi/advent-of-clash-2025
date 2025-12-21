{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments #-}
module Main where

import Options.Applicative
import Data.Traversable
import Data.Char (digitToInt)

data Options = Options
    { inFile :: FilePath
    }

opts :: Parser Options
opts = do
    inFile <- strOption . mconcat $
      [ long "input"
      , short 'i'
      , help "input filename"
      ]
    pure Options{..}

type Digit = Int

solve :: (Ord a, Bounded a) => [a] -> (a, a)
solve xs = (x, y)
  where
    (_, rs) = mapAccumR (\acc x -> let acc' = acc `max` x in (acc', acc')) minBound xs
    (_, ls) = mapAccumL (\acc x -> let acc' = acc `max` x in (acc', acc)) minBound xs
    (x, y) = maximum $ zip ls rs

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc
    problems <- lines <$> readFile inFile
    s <- sum <$> for problems \prob -> do
        let (x, y) = solve (map digitToInt prob)
            val = (x * 10 + y)
        pure val
    print s
