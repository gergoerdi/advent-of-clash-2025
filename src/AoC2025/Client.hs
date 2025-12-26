-- Based on https://codeberg.org/TristanCacqueray/advent-of-clash/src/branch/main/app/Client.hs

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, ApplicativeDo, RecordWildCards
#-}

module AoC2025.Client where

import Prelude
import Control.Monad (unless)
import Data.Foldable (for_)

import Data.ByteString qualified as BS
import System.Hardware.Serialport qualified as S
import Clash.Format (ascii)
import Options.Applicative

data Options = Options
    { inFile :: FilePath
    , dev :: FilePath
    }

opts :: Parser Options
opts = do
    inFile <- strOption . mconcat $
      [ long "input"
      , short 'i'
      , help "input filename"
      ]

    dev <- strOption . mconcat $
      [ long "port"
      , short 'p'
      , help "serial device port"
      ]

    pure Options{..}

main :: IO ()
main = do
    Options{..} <- execParser $ info (opts <**> helper) fullDesc

    let settings = S.defaultSerialSettings
          { S.timeout = 60
          , S.commSpeed = S.CS115200
          }

    inputs <- BS.split (ascii '\n') <$> BS.readFile inFile

    S.withSerial dev settings \port -> for_ inputs \input -> do
        sendAll port input
        S.send port "\n"
        BS.putStr =<< recvLine port
  where
    sendAll port s = do
        sent <- S.send port s
        let remaining = BS.drop sent s
        unless (BS.null remaining) $ sendAll port remaining

    recvLine port = do
        bs <- S.recv port 1
        case BS.unpack bs of
            [] -> pure bs
            [eol] | eol == ascii '\n' -> pure bs
            _ -> (bs <>) <$> recvLine port
