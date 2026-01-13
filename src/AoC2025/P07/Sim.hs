{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
module AoC2025.P07.Sim where

import Clash.Prelude
import Options.Applicative

import AoC2025.P07.TopEntity (Valid, board)
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

type LineLen = 141

sim_board :: forall n k -> Valid n k => String -> String
sim_board n k = simCircuitUntilFinalLine @System (board n k)

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc

    problems <- readFile inFile
    putStr $ sim_board LineLen 48 $ problems
