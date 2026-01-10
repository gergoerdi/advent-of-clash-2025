{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE RequiredTypeArguments #-}
module AoC2025.P07.Control (Valid, controller) where

import Clash.Prelude
import Protocols

import AoC2025.BCD
import AoC2025.Control hiding (controller)
import qualified AoC2025.Control as Ctl
import AoC2025.P07.Solve
import Control.Monad.State.Strict

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

data Thought k
    = FinishLine
    | ToBCD OutputPart (Index k)
    deriving (Generic, NFDataX, Show)

data St n k = St
    { phase :: Phase (Input n) (Output k) (Thought k)
    , row :: Vec n (Unsigned k)
    , rowState :: RowSt (Unsigned k) (Unsigned k)
    , bcdConverter :: ShiftAdd k
    , bcdOutput :: BCD (BCDSize k)
    }
    deriving (Generic, NFDataX, Show)

controller
    :: forall dom. (HiddenClockResetEnable dom)
    => forall n k -> Valid n k
    => Circuit (Df dom Bool) (Df dom (Maybe Digit))
controller n k = Ctl.controller s0 \ (in_data, out_ack) -> gets phase >>= handle in_data out_ack
    (\case
        Start i -> \source -> do
            modify \st -> st
                { phase = next (Read . Start) i $ Read $ Line 0
                , row = row st <<+ if source then 1 else 0
                , rowState = initRow
                }

        Line i -> \splitter -> do
            rowState <- gets rowState
            above <- gets $ leToPlus @1 @n head . row
            let (below, rowState') = runState (propagateCell splitter above) rowState
            modify \st -> st
                { phase = next (Read . Line) i (Think FinishLine)
                , row = row st <<+ below
                , rowState = rowState'
                })

    (\case
        Digit part i -> do
            d <- gets $ leToPlus @1 @(BCDSize k) head . bcdOutput
            modify \st -> st
                { phase = next (Write . Digit part) i (Write $ EOL part)
                , bcdOutput = bcdOutput st <<+ 0
                }
            pure $ Just d

        EOL part -> do
            case part of
                Hits -> modify \st -> st
                    { phase = Think $ ToBCD Timelines 0
                    , bcdConverter = initBCD (timelineSum . rowState $ st)
                    , rowState = newRow (rowState st)
                    }
                Timelines -> goto $ Read $ Line 0
            pure Nothing)

    (\case
        FinishLine -> do
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

        ToBCD part i -> do
            bcd <- gets bcdConverter
            let bcd' = stepBCD bcd
            modify \st -> st
                { phase = next (Think . ToBCD part) i (Write $ Digit part 0)
                , bcdConverter = bcd'
                , bcdOutput = finishBCD bcd'
                })
  where
    s0 :: St n k
    s0 = St
      { phase = Read $ Start 0
      , row = repeat undefined
      , rowState = undefined
      , bcdConverter = undefined
      , bcdOutput = undefined
      }

    goto ph = modify \st -> st{ phase = ph }
