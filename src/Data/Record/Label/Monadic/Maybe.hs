{-# LANGUAGE TypeOperators #-}
module Data.Record.Label.Monadic.Maybe
(
-- * Lens variants of common 'MonadState' operations.
  get

-- * Lens variants of common 'MonadReader' operations.
, ask
)
where

import Control.Monad
import Data.Record.Label.Maybe ((:~>))
import qualified Control.Monad.Reader    as R
import qualified Control.Monad.State     as S
import qualified Data.Record.Label.Maybe as L

-- | Get a value out of state pointed to by the specified lens that might fail.
-- When the lens getter fails this computation will fall back to mzero.

get :: (S.MonadState f m, MonadPlus m) => (f :~> b) -> m b
get l = (L.get l `liftM` S.get) >>= (mzero `maybe` return)

-- | Fetch a value pointed to by a lens that might fail out of a reader
-- environment. When the lens getter fails this computation will fall back to
-- mzero.

ask :: (R.MonadReader f m, MonadPlus m) => (f :~> a) -> m a
ask l = (L.get l `liftM` R.ask) >>= (mzero `maybe` return)

