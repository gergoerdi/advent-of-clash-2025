{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
module AoC2025.P03.Sim where

import Clash.Prelude
import Options.Applicative

import AoC2025.P03.TopEntity (Valid, board)
import AoC2025.Serial

data Part = Part1 | Part2

data Options = Options
    { inFile :: FilePath
    , part :: Part
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

    pure Options{..}

type LineLen = 100

sim_board :: forall n k m -> Valid n k m => String -> String
sim_board n k m = simCircuitUntilFinalLine @System (board n k m)

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc

    let sim = case part of
            Part1 -> sim_board 100 2 5
            Part2 -> sim_board 100 12 15

    problems <- readFile inFile
    putStr $ sim problems
