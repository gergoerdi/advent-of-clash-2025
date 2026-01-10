{-# LANGUAGE LambdaCase, BlockArguments #-}
module AoC2025.Control where

import Clash.Prelude
import Clash.Class.Counter
import Control.Monad.State.Strict
import Protocols
import qualified Protocols.Df as Df
import Clash.Format (countSuccChecked)

next :: (Counter a) => (a -> s) -> a -> s -> s
next cons i after = maybe after cons $ countSuccChecked i

data Phase a b c
    = Read a
    | Write b
    | Think c
    deriving (Generic, NFDataX, Show)

data Control o
    = Busy
    | Consume
    | Produce o
    deriving (Show)

wait :: Ack -> State s a -> State s a
wait ack act = do
    s0 <- get
    result <- act
    s <- get

    -- This is written in a weird way because the effects cannot
    -- depend on `ack`, only their parameters. So we always have to
    -- `put`, even if we don't need to roll back.
    let s' = case ack of
            Ack True -> s
            Ack False -> s0
    put s'
    pure result

handle
    :: (f ~ State s)
    => Df.Data i
    -> Ack
    -> (a -> i -> f ())
    -> (b -> f o)
    -> (c -> f ())
    -> Phase a b c
    -> f (Control o)
handle in_data out_ack read write think = \case
    Read s -> case in_data of
        Df.NoData -> pure Busy
        Df.Data input -> do
            read s input
            pure Consume

    Write s -> do
        output <- wait out_ack $ write s
        pure $ Produce output

    Think s -> do
        think s
        pure Busy

controller
    :: (HiddenClockResetEnable dom, NFDataX s)
    => s
    -> ((Df.Data i, Ack) -> State s (Control o))
    -> Circuit (Df dom i) (Df dom o)
controller s0 control = Circuit $ mealySB (fmap lines . control) s0
  where
    lines = \case
        Busy -> (Ack False, Df.NoData)
        Consume -> (Ack True, Df.NoData)
        Produce o -> (Ack False, Df.Data o)
