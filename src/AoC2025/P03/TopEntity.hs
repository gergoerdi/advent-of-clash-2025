{-# LANGUAGE NumericUnderscores #-}
module AoC2025.P03.TopEntity where

import Clash.Prelude
import Clash.Annotations.TH

createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

topEntity
    :: "CLK_100MHZ" ::: Clock Dom100
    -> "RESET"      ::: Reset Dom100
    -> "RX"         ::: Signal Dom100 Bit
    -> "TX"         ::: Signal Dom100 Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    \rx -> register 0 rx

makeTopEntity 'topEntity
