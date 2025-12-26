{-# LANGUAGE NumericUnderscores, LambdaCase, BlockArguments #-}
{-# LANGUAGE RequiredTypeArguments #-}
module AoC2025.P03.Control (Valid, controller) where

import Clash.Prelude
import Clash.Class.Counter

import AoC2025.P03.BCD
import AoC2025.P03.Solve

import Control.Monad.State.Strict
import Data.Ord (Down)

import Protocols
import qualified Protocols.Df as Df
import Clash.Format (countSuccChecked)

next :: (Counter a) => (a -> s) -> a -> s -> s
next cons i after = maybe after cons $ countSuccChecked i

type Valid n k m = (KnownNat n, KnownNat k, KnownNat m, 1 <= n, 1 <= k, 1 <= m, k <= m, k <= n)

data Phase n k m
    = ShiftIn (Index n)
    | Prepare (Index n) (Index n) (Index k)
    | Calculate (Index n) (Index k) (Index (CLog 2 n + 1))
    | Add Bit (Index m)
    | ShiftOut (Index m)
    | ShiftOutEOL
    deriving (Generic, NFDataX, Show)

data St n k m = St
    { phase :: Phase n k m
    , row :: BCD n
    , buf :: Vec n (Bool, Digit, Down (Index n))
    , curr :: BCD m
    , acc :: BCD m
    }
    deriving (Generic, NFDataX, Show)


data Control
    = Busy
    | Consume Digit
    | Produce (Maybe Digit)
    deriving (Show)

control :: forall n k m. Valid n k m => (Df.Data Digit, Ack) -> State (St n k m) Control
control (shift_in, out_ack) = gets phase >>= \case
    ShiftIn i -> case shift_in of
        Df.NoData -> do
            pure Busy
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
        pure Busy
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
                    { curr = replace (resize i + fromSNat (SNat @(m - k)) :: Index m) x (curr st)
                    , phase = next (Prepare start' end') i (Add 0 0)
                    }
        pure Busy
    Add cin i -> do
        d1 <- gets $ leToPlus @1 @m last . acc
        d2 <- gets $ leToPlus @1 @m last . curr
        let (cout, d) = addDigit cin (d1, d2)
        modify \st -> st
            { acc = replace (maxBound :: Index m) d (acc st) `rotateRightS` SNat @1
            , curr = 0 +>> curr st
            , phase = next (Add cout) i $ ShiftOut 0
            }
        pure Busy
    ShiftOut i -> do
        d <- gets $ leToPlus @1 @m head . acc
        wait out_ack do
            modify \st -> st
              { phase = next ShiftOut i ShiftOutEOL
              , acc = rotateLeftS (acc st) (SNat @1)
              }
        pure $ Produce (Just d)
    ShiftOutEOL -> do
        wait out_ack $ goto $ ShiftIn 0
        pure $ Produce Nothing
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
    => forall n k m -> Valid n k m
    => Circuit (Df dom Digit) (Df dom (Maybe Digit))
controller n k m = Circuit $ mealySB (fmap lines . control) s0
  where
    s0 = St
      { phase = ShiftIn @n @k @m 0
      , row = repeat undefined
      , buf = repeat undefined
      , curr = repeat 0
      , acc = repeat 0
      }

    lines = \case
        Busy -> (Ack False, Df.NoData)
        Consume d -> (Ack True, Df.NoData)
        Produce d -> (Ack False, Df.Data d)
