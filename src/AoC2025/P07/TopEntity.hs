{-# LANGUAGE NumericUnderscores, LambdaCase, BlockArguments #-}
{-# LANGUAGE RequiredTypeArguments #-}
module AoC2025.P07.TopEntity (board, topEntity, Valid) where

import Clash.Prelude hiding (print)
import Clash.Annotations.TH
import Clash.Class.Counter

import AoC2025.Serial
import AoC2025.BCD (Digit)
import AoC2025.P07.Control

import Data.Word (Word8)

import Protocols
import qualified Protocols.Df as Df
import Clash.Format

next :: (Counter a) => (a -> s) -> a -> s -> s
next cons i after = maybe after cons $ countSuccChecked i

board
    :: (HiddenClockResetEnable dom)
    => forall n k -> Valid n k
    => Circuit (Df dom Word8) (Df dom Word8)
board n k =
    Df.mapMaybe parse |>
    controller n k |>
    Df.map (maybe (ascii '#') showDigit) |>
    format (loop $ number <> str " " <> number <> str "\r\n")
  where
    parse c
        | c `elem` (ascii <$> ['S', '^'])
        = Just True

        | c == ascii '.'
        = Just False

        | otherwise
        = Nothing

    number = skip (ascii '0') <> delimit (ascii '#') print

showDigit :: Digit -> Word8
showDigit d = fromIntegral d + ascii '0'

createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

topEntity
    :: "CLK_100MHZ" ::: Clock Dom100
    -> "RESET"      ::: Reset Dom100
    -> "RX"         ::: Signal Dom100 Bit
    -> "TX"         ::: Signal Dom100 Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    serialize SerialRate $ board 141 48

makeTopEntity 'topEntity
