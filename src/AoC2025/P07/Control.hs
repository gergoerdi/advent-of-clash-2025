{-# LANGUAGE NumericUnderscores, LambdaCase, BlockArguments #-}
{-# LANGUAGE RequiredTypeArguments #-}
module AoC2025.P07.Control (Valid, controller) where

import Clash.Prelude
import Clash.Class.Counter
import Data.Word

import AoC2025.BCD
import AoC2025.P07.Solve

import Control.Monad.State.Strict
import Data.Ord (Down)

import Protocols
import qualified Protocols.Df as Df
import Clash.Format (countSuccChecked)

import Debug.Trace

next :: (Counter a) => (a -> s) -> a -> s -> s
next cons i after = maybe after cons $ countSuccChecked i

type Valid n k = (KnownNat n, 1 <= n, KnownNat k, 1 <= k, 1 <= BCDSize k)

data Input n
    = Start (Index n)
    | Line (Index n)
    deriving (Generic, NFDataX, Show)

data Output k
    = HitsDigit (Index (BCDSize k))
    | HitsEOL
    | WorldsDigit (Index (BCDSize k))
    | WorldsEOL
    deriving (Generic, NFDataX, Show)

data Phase n k
    = Read (Input n)
    | FinishLine
    | Write (Output k)
    deriving (Generic, NFDataX, Show)

data St n k = St
    { phase :: Phase n k
    , row :: Vec n (Unsigned k)
    , rowState :: (Unsigned k, Unsigned k, Unsigned k)
    , hits :: BCD (BCDSize k)
    , worlds :: BCD (BCDSize k)
    }
    deriving (Generic, NFDataX, Show)

data Control a
    = Busy
    | Consume
    | Produce a
    deriving (Show)

control :: forall n k. Valid n k => (Df.Data Bool, Ack) -> State (St n k) (Control (Maybe Digit))
control (in_data, out_ack) = gets phase >>= {- (\x -> traceShowM x *> pure x) >>= -} \case
    Read (Start i) -> case in_data of
        Df.NoData -> pure Busy
        Df.Data source -> do
            modify \st -> st
                { phase = next (Read . Start) i $ Read $ Line 0
                , row = row st <<+ if source then 1 else 0
                , rowState = (0, 0, 0)
                }
            pure Consume

    Read (Line i) -> case in_data of
        Df.NoData -> pure Busy
        Df.Data splitter -> do
            rowState <- gets rowState
            above <- gets $ leToPlus @1 @n head . row
            let (below, rowState') = runState (propagateCell splitter above) rowState
            modify \st -> st
                { phase = next (Read . Line) i FinishLine
                , row = row st <<+ below
                , rowState = rowState'
                }
            pure Consume

    FinishLine -> do
        rowState@(fromLeft1, _, hits) <- gets rowState
        row <- gets row
        let row' = row <<+ fromLeft1
        let worlds = sum row'
        modify \st -> st
            { phase = Write $ HitsDigit 0
            , row = row'
            , rowState = (0, 0, hits)
            , hits = toBCD hits
            , worlds = toBCD worlds
            }
        pure Busy

    Write (HitsDigit i) -> do
        d <- gets $ leToPlus @1 @(BCDSize k) head . hits
        wait out_ack do
            modify \st -> st
                { phase = next (Write . HitsDigit) i (Write HitsEOL)
                , hits = hits st <<+ 0
                }
        pure $ Produce $ Just d

    Write HitsEOL -> do
        wait out_ack do
            goto $ Write $ WorldsDigit 0
        pure $ Produce Nothing

    Write (WorldsDigit i) -> do
        d <- gets $ leToPlus @1 @(BCDSize k) head . worlds
        wait out_ack do
            modify \st -> st
                { phase = next (Write . WorldsDigit) i (Write WorldsEOL)
                , worlds = worlds st <<+ 0
                }
        pure $ Produce $ Just d

    Write WorldsEOL -> do
        wait out_ack do
            goto $ Read $ Line 0
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
    => forall n k -> Valid n k
    => Circuit (Df dom Bool) (Df dom (Maybe Digit))
controller n k = Circuit $ mealySB (fmap lines . control) s0
  where
    s0 = St
      { phase = Read @n @k $ Start 0
      , row = repeat undefined
      , rowState = undefined
      , hits = undefined
      , worlds = undefined
      }

    lines = \case
        Busy -> (Ack False, Df.NoData)
        Consume -> (Ack True, Df.NoData)
        Produce d -> (Ack False, Df.Data d)
