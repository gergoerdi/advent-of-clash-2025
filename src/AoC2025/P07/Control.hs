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

data OutputPart
    = Hits
    | Timelines
    deriving (Generic, NFDataX, Show)

data Output k
    = Digit OutputPart (Index (BCDSize k))
    | EOL OutputPart
    deriving (Generic, NFDataX, Show)

data Phase n k
    = Read (Input n)
    | Think (Thought k)
    | Write (Output k)
    deriving (Generic, NFDataX, Show)

data Thought k
    = FinishLine
    | ToBCD OutputPart (Index k)
    deriving (Generic, NFDataX, Show)

data St n k = St
    { phase :: Phase n k
    , row :: Vec n (Unsigned k)
    , rowState :: RowSt (Unsigned k) (Unsigned k)
    , bcdConverter :: ShiftAdd k
    , bcdOutput :: BCD (BCDSize k)
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
                , rowState = initRow
                }
            pure Consume

    Read (Line i) -> case in_data of
        Df.NoData -> pure Busy
        Df.Data splitter -> do
            rowState <- gets rowState
            above <- gets $ leToPlus @1 @n head . row
            let (below, rowState') = runState (propagateCell splitter above) rowState
            modify \st -> st
                { phase = next (Read . Line) i (Think FinishLine)
                , row = row st <<+ below
                , rowState = rowState'
                }
            pure Consume

    Think FinishLine -> do
        fromLeft1 <- gets $ head . buffer . rowState
        hits <- gets $ hitCount . rowState
        timelines <- gets $ timelineSum . rowState
        let timelines' = timelines + fromLeft1
        modify \st -> st
            { phase = Think $ ToBCD Hits 0
            , row = row st <<+ fromLeft1
            , rowState = (rowState st){ timelineSum = timelines' }
            , bcdConverter = initBCD hits
            }
        pure Busy

    Think (ToBCD part i) -> do
        bcd <- gets bcdConverter
        let bcd' = stepBCD bcd
        modify \st -> st
            { phase = next (Think . ToBCD part) i (Write $ Digit part 0)
            , bcdConverter = bcd'
            , bcdOutput = finishBCD bcd'
            }
        pure Busy

    Write (Digit part i) -> do
        d <- gets $ leToPlus @1 @(BCDSize k) head . bcdOutput
        wait out_ack do
            modify \st -> st
                { phase = next (Write . Digit part) i (Write $ EOL part)
                , bcdOutput = bcdOutput st <<+ 0
                }
        pure $ Produce $ Just d

    Write (EOL part) -> do
        wait out_ack $ case part of
            Hits -> modify \st -> st
                { phase = Think $ ToBCD Timelines 0
                , bcdConverter = initBCD (timelineSum . rowState $ st)
                , rowState = newRow (rowState st)
                }
            Timelines -> goto $ Read $ Line 0
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
      , bcdConverter = undefined
      , bcdOutput = undefined
      }

    lines = \case
        Busy -> (Ack False, Df.NoData)
        Consume -> (Ack True, Df.NoData)
        Produce d -> (Ack False, Df.Data d)
