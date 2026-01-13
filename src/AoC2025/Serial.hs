{-# LANGUAGE BlockArguments, TupleSections, NumericUnderscores, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
module AoC2025.Serial where

import Clash.Prelude hiding (lift)

import Protocols
import qualified Protocols.Df as Df
import Clash.Cores.UART(uart, ValidBaud)
import Control.Arrow.Transformer.Automaton
import Data.Char (chr)
import Data.Word
import Clash.Format (ascii)

type SerialRate = 115_200

-- From git@github.com:bittide/bittide-hardware.git
uartDf
    :: (HiddenClockResetEnable dom, ValidBaud dom baud)
    => SNat baud
    -> Circuit
        (Df dom (BitVector 8), CSignal dom Bit)
        (CSignal dom (Maybe (BitVector 8)), CSignal dom Bit)
uartDf baud = Circuit \((request, rx_bit), _out_ack) ->
    let (received, tx_bit, in_ack) = uart baud rx_bit (Df.dataToMaybe <$> request)
    in ((Ack <$> in_ack, pure ()), (received, tx_bit))

buffer :: (HiddenClockResetEnable dom, NFDataX a) => Circuit (CSignal dom (Maybe a)) (Df dom a)
buffer = Circuit \(x, ack) ->
    let r = register Nothing do
            current <- r
            next <- x
            ~(Ack ack) <- ack
            pure $ if ack then next else current <|> next
    in (pure (), Df.maybeToData <$> r)

uncircuit :: Circuit (CSignal dom a) (CSignal dom b) -> Signal dom a -> Signal dom b
uncircuit c = snd . toSignals c . (, pure ())

serialize
    :: (HiddenClockResetEnable dom, BitPack a, BitSize a ~ 8, BitPack b, BitSize b ~ 8)
    => forall baud -> (KnownNat baud, ValidBaud dom baud)
    => Circuit (Df dom a) (Df dom b)
    -> Signal dom Bit
    -> Signal dom Bit
serialize baud par_circuit = uncircuit $ circuit \rx -> do
    (in_byte, tx) <- uartDf (SNat @baud) -< (out_byte, rx)
    out_byte <- Df.map pack <| par_circuit <| Df.map unpack <| buffer -< in_byte
    idC -< tx

simCircuit :: (KnownDomain dom) => (o -> Bool) -> (HiddenClockResetEnable dom => Circuit (Df dom i) (Df dom o)) -> [i] -> [o]
simCircuit finish circuit = init sim
  where
    sim = signalAutomaton $ bundle . toSignals circuit . unbundle

    init (Automaton step) = feed sim'
      where
        ((ack, out), sim') = step (Df.NoData, Ack False)

    feed sim@(Automaton step) = \case
        (x:xs) -> prepend out $ feed sim' $ case ack of
            Ack False -> x:xs
            Ack True -> xs
          where
            ((ack, out), sim') = step (Df.Data x, Ack True)
        [] -> consume sim

    consume (Automaton step) = prepend out $ case out of
        Df.Data x | finish x -> []
        _ -> consume sim'
      where
        ((ack, out), sim') = step (Df.NoData, Ack True)

    prepend = \case
        Df.NoData -> id
        Df.Data x -> (x:)

simCircuitUntilFinalLine :: (KnownDomain dom) => (HiddenClockResetEnable dom => Circuit (Df dom Word8) (Df dom Word8)) -> String -> String
simCircuitUntilFinalLine circuit =
    fmap (chr . fromIntegral) .
    simCircuit (== ascii '\n') circuit .
    fmap ascii
