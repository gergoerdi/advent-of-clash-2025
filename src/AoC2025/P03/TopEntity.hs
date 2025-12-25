{-# LANGUAGE NumericUnderscores, LambdaCase, BlockArguments #-}
{-# LANGUAGE RequiredTypeArguments #-}
module AoC2025.P03.TopEntity where

import Clash.Prelude
import Clash.Annotations.TH
import Clash.Class.Counter

import AoC2025.Serial
import AoC2025.P03.BCD
import AoC2025.P03.Solve

import Data.Word (Word8)
import Data.Char (ord, chr)
import Control.Monad (when)
import Control.Monad.State.Strict
import Data.Ord (Down)

import Protocols
import qualified Protocols.Df as Df

import Protocols.Internal (simulateCSE)

countSuccChecked :: (Counter a) => a -> Maybe a
countSuccChecked x = case countSucc ((0 :: Unsigned 1), x) of
    (0, x') -> Just x'
    (1, _) -> Nothing

next :: (Counter a) => (a -> s) -> a -> s -> s
next cons i after = maybe after cons $ countSuccChecked i

ascii :: Char -> Word8
ascii c
    | code <= 0x7f = fromIntegral code
    | otherwise = clashCompileError "Not an ASCII code point"
  where
    code = ord c

type Stream dom a b = (Signal dom (Df.Data a), Signal dom Ack) -> (Signal dom Ack, Signal dom (Df.Data b))

type Valid n k l = (KnownNat n, KnownNat k, KnownNat l, 1 <= n, 1 <= k, 1 <= l, k <= l, k <= n)

board
    :: (HiddenClockResetEnable dom)
    => forall n k l -> Valid n k l
    => Circuit (Df dom Word8) (Df dom Word8)
board n k l =
    Df.mapMaybe parseDigit |>
    Circuit (controller n k l) |>
    Df.map showDigit

parseDigit :: Word8 -> Maybe Digit
parseDigit x
    | ascii '0' <= x && x <= ascii '9' = Just $ fromIntegral $ x - ascii '0'
    | otherwise = Nothing

showDigit :: Digit -> Word8
showDigit d = fromIntegral d + ascii '0'

data Phase n k l
    = ShiftIn (Index n)
    | Prepare (Index n) (Index n) (Index k)
    | Calculate (Index n) (Index k) (Index (CLog 2 n + 1))
    | Add
    | ShiftOut (Index l)
    | ShiftOutNewline
    deriving (Generic, NFDataX, Show)

data St n k l = St
    { phase :: Phase n k l
    , row :: BCD n
    , buf :: Vec n (Bool, Digit, Down (Index n))
    , curr :: BCD k
    , acc :: BCD l
    }
    deriving (Generic, NFDataX, Show)

control :: forall n k l. Valid n k l => (Df.Data Digit, Ack) -> State (St n k l) Control
control (shift_in, out_ack) = gets phase >>= \case
    ShiftIn i -> case shift_in of
        Df.NoData -> do
            pure Wait
        Df.Data shift_in -> do
            modify \st -> st
              { phase = next ShiftIn i $ Prepare 0 (fromSNat (SNat @(n - k))) 0
              , row = replace i shift_in (row st)
              }
            pure $ Consume shift_in
    Prepare start end i -> do
        modify \st -> st
            { phase = Calculate end i 0
            , buf = imap (score start end) (row st)
            }
        pure Wait
    Calculate end i j -> do
        case countSuccChecked j of
            Just j' -> do
                modify \st -> st
                    { buf = rollup max (buf st)
                    , phase = Calculate end i j'
                    }
            Nothing -> do
                buf <- gets buf
                let (idx, x) = unscore $ leToPlus @1 @n head buf
                    start' = idx + 1
                    end' = end + 1
                modify \st -> st
                    { curr = replace (resize i + fromSNat (SNat @(l - k)) :: Index l) x (curr st)
                    , phase = next (Prepare start' end') i Add
                    }
        pure Wait
    Add -> do
        modify \st -> st{ acc = addBCD (acc st) (curr st) }
        goto $ ShiftOut 0
        pure Wait
    ShiftOut i -> do
        proceed <- wait out_ack $ goto $ next ShiftOut i (ShiftIn 0)
        d <- gets $ leToPlus @1 @l head . acc
        when proceed $ modify \st -> st{ acc = rotateLeftS (acc st) (SNat @1) }
        pure $ Produce d
  where
    goto ph = modify \st -> st{ phase = ph }

    wait ack act = do
        s0 <- get
        s <- act *> get
        let (proceed, s') = case ack of
                Ack True -> (True, s)
                Ack False -> (False, s0)
        put s'
        pure proceed

controller
    :: forall dom. (HiddenClockResetEnable dom)
    => forall n k l -> Valid n k l
    => Stream dom Digit Digit
controller n k l (shift_in, out_ack) = (in_ack, shift_out)
  where
    (shift_out, in_ack) = mealySB (fmap lines . control) s0 (shift_in, out_ack)
    s0 = St
      { phase = ShiftIn 0
      , row = repeat @n undefined
      , buf = repeat @n undefined
      , curr = repeat @k undefined
      , acc = repeat @l 0
      }

    lines = \case
        Wait -> (Df.NoData, Ack False)
        Consume d -> (Df.NoData, Ack True)
        Produce d -> (Df.Data d, Ack False)

data Control
    = Wait
    | Consume Digit
    | Busy
    | Produce Digit

createDomain vSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

topEntity
    :: "CLK_100MHZ" ::: Clock Dom100
    -> "RESET"      ::: Reset Dom100
    -> "RX"         ::: Signal Dom100 Bit
    -> "TX"         ::: Signal Dom100 Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    serialize 9600 $ board 100 12 20

makeTopEntity 'topEntity

sim_board :: forall n k l -> Valid n k l => String -> String
sim_board n k l =
    fmap (chr . fromIntegral) .
    simulateCSE @System (exposeClockResetEnable $ board n k l) .
    fmap ascii
