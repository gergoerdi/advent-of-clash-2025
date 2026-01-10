{-# LANGUAGE NumericUnderscores, LambdaCase, BlockArguments #-}
{-# LANGUAGE RequiredTypeArguments #-}
module AoC2025.P03.Control (Valid, controller) where

import Clash.Prelude
import Protocols

import AoC2025.BCD
import AoC2025.Control hiding (controller)
import qualified AoC2025.Control as Ctl
import AoC2025.P03.Solve

import Control.Monad.State.Strict
import Data.Ord (Down)

import Clash.Format (countSuccChecked)

type Valid n k m = (KnownNat n, KnownNat k, KnownNat m, 1 <= n, 1 <= k, 1 <= m, k <= m, k <= n)

data Output m
    = Digit (Index m)
    | EOL
    deriving (Generic, NFDataX, Show)

data Thought m n k
    = Prepare (Index n) (Index n) (Index k)
    | Calculate (Index n) (Index k) (Index (CLog 2 n + 1))
    | Add Bit (Index m)
    deriving (Generic, NFDataX, Show)

data St n k m = St
    { phase :: Phase (Index n) (Output m) (Thought m n k)
    , row :: BCD n
    , buf :: Vec n (Bool, Digit, Down (Index n))
    , curr :: BCD m
    , acc :: BCD m
    }
    deriving (Generic, NFDataX, Show)

controller
    :: forall dom. (HiddenClockResetEnable dom)
    => forall n k m -> Valid n k m
    => Circuit (Df dom Digit) (Df dom (Maybe Digit))
controller n k m = Ctl.controller s0 \ (in_data, out_ack) -> gets phase >>= handle in_data out_ack
    (\i shift_in -> do
        modify \st -> st
            { phase = next Read i $ Think $ Prepare 0 (fromSNat (SNat @(n - k))) 0
            , row = replace i shift_in (row st)
            })
    (\case
        Digit i -> do
            d <- gets $ leToPlus @1 @m head . acc
            modify \st -> st
              { phase = next (Write . Digit) i (Write EOL)
              , acc = rotateLeftS (acc st) (SNat @1)
              }
            pure $ Just d

        EOL -> do
            goto $ Read 0
            pure Nothing)

    (\case
        Prepare start end i -> do
            modify \st -> st
                { phase = Think $ Calculate end i 0
                , buf = imap (score start end) (row st)
                }

        Calculate end i j -> do
            case countSuccChecked j of
                Just j' -> do
                    modify \st -> st
                        { buf = rollup max (buf st)
                        , phase = Think $ Calculate end i j'
                        }
                Nothing -> do
                    buf <- gets buf
                    let (idx, x) = unscore $ leToPlus @1 @n head buf
                        start' = idx + 1
                        end' = end + 1
                    modify \st -> st
                        { curr = replace (resize i + fromSNat (SNat @(m - k)) :: Index m) x (curr st)
                        , phase = next (Think . Prepare start' end') i (Think $ Add 0 0)
                        }

        Add cin i -> do
            d1 <- gets $ leToPlus @1 @m last . acc
            d2 <- gets $ leToPlus @1 @m last . curr
            let (cout, d) = addDigit cin (d1, d2)
            modify \st -> st
                { acc = replace (maxBound :: Index m) d (acc st) `rotateRightS` SNat @1
                , curr = 0 +>> curr st
                , phase = next (Think . Add cout) i $ (Write $ Digit 0)
                })
  where
    s0 :: St n k m
    s0 = St
      { phase = Read 0
      , row = repeat undefined
      , buf = repeat undefined
      , curr = repeat 0
      , acc = repeat 0
      }

    goto ph = modify \st -> st{ phase = ph }

-- controller
--     :: forall dom. (HiddenClockResetEnable dom)
--     => forall n k m -> Valid n k m
--     => Circuit (Df dom Digit) (Df dom (Maybe Digit))
-- controller n k m = Circuit $ mealySB (fmap lines . control) s0
--   where
--     s0 = St
--       { phase = ShiftIn @n @k @m 0
--       , row = repeat undefined
--       , buf = repeat undefined
--       , curr = repeat 0
--       , acc = repeat 0
--       }

--     lines = \case
--         Busy -> (Ack False, Df.NoData)
--         Consume d -> (Ack True, Df.NoData)
--         Produce d -> (Ack False, Df.Data d)
