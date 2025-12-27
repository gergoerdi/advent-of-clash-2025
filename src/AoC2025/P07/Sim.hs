{-# LANGUAGE ApplicativeDo, RecordWildCards, BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
module AoC2025.P07.Sim where

import Clash.Prelude
import Options.Applicative

import AoC2025.P07.TopEntity (Valid, board)
import Protocols.Internal (simulateCSE)
import Clash.Format (ascii)
import Data.Char (chr)

sim_board :: forall n k -> Valid n k => String -> String
sim_board n k =
    fmap (chr . fromIntegral) .
    simulateCSE @System (exposeClockResetEnable $ board n k) .
    fmap ascii

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

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc

    problems <- readFile inFile
    putStr $ sim_board 141 64 $ problems
