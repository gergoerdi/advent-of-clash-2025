{-# LANGUAGE ApplicativeDo, RecordWildCards #-}
module AoC2025.CLI where

import Clash.Prelude
import Options.Applicative

data Part = Part1 | Part2

data Options = Options
    { inFile :: FilePath
    , part :: Part
    , verbose :: Bool
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

    verbose <- switch . mconcat $
      [ long "verbose"
      ]

    pure Options{..}

getOptions :: IO Options
getOptions = execParser $ info (opts <**> helper) fullDesc
