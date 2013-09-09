{-| State and Reader operations specialized for working with partial lenses.
Failure is handled by embedding the Nothing in `mzero` from `MonadPlus`.
-}

{-# LANGUAGE TypeOperators #-}

module Data.Label.PartialM
( gets
, asks
)
where

import Control.Monad
import Data.Label.Partial

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State  as State

-- | Get a value out of state, pointed to by the specified lens that might
-- fail. When the lens getter fails this computation will fall back to
-- `mzero'.

gets :: (State.MonadState f m, MonadPlus m) => (f -> g) :~> (o -> i) -> m o
gets l = State.gets (get l) >>= (mzero `maybe` return)

-- | Fetch a value, pointed to by a lens that might fail, out of a reader
-- environment. When the lens getter fails this computation will fall back to
-- `mzero'.

asks :: (Reader.MonadReader f m, MonadPlus m) => (f -> g) :~> (o -> i) -> m o
asks l = Reader.asks (get l) >>= (mzero `maybe` return)

