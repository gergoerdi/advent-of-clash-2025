{-# LANGUAGE NumericUnderscores, LambdaCase, BlockArguments #-}
{-# LANGUAGE RequiredTypeArguments #-}
module AoC2025.P03.TopEntity (board, topEntity, Valid) where

import Clash.Prelude hiding (print)
import Clash.Annotations.TH
import Clash.Class.Counter
import Data.Word

import AoC2025.Serial
import AoC2025.BCD (Digit)
import AoC2025.P03.Control

import Protocols
import qualified Protocols.Df as Df
import Clash.Format

next :: (Counter a) => (a -> s) -> a -> s -> s
next cons i after = maybe after cons $ countSuccChecked i

board
    :: (HiddenClockResetEnable dom)
    => forall n k m -> Valid n k m
    => Circuit (Df dom Word8) (Df dom Word8)
board n k m =
    Df.mapMaybe parseDigit |>
    controller n k m |>
    Df.map (maybe (ascii '#') showDigit) |>
    format (loop $ skip (ascii '0') <> delimit (ascii '#') print <> str "\r\n")

parseDigit :: Word8 -> Maybe Digit
parseDigit x
    | ascii '0' <= x && x <= ascii '9' = Just $ fromIntegral $ x - ascii '0'
    | otherwise = Nothing

showDigit :: Digit -> Word8
showDigit d = fromIntegral d + ascii '0'

createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

topEntity
    :: "CLK_100MHZ" ::: Clock Dom100
    -> "RESET"      ::: Reset Dom100
    -> "RX"         ::: Signal Dom100 Bit
    -> "TX"         ::: Signal Dom100 Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    serialize SerialRate $ board 100 12 15

makeTopEntity 'topEntity
