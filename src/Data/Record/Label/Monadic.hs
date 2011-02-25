{-# LANGUAGE TypeOperators, TypeSynonymInstances, TemplateHaskell #-}
module Data.Record.Label.Monadic
(
-- * Lens variants of common 'MonadState' operations.
  get
, put
, modify
, (=:)

-- * Lens variants of common 'MonadReader' operations.
, ask
, local
)
where

import Data.Record.Label.Pure ((:->))
import qualified Control.Monad.Reader   as R
import qualified Control.Monad.State    as S
import qualified Data.Record.Label.Pure as L

-- | Get a value out of state pointed to by the specified lens.

get :: S.MonadState s m => s :-> b -> m b
get = S.gets . L.get

-- | Set a value somewhere in state pointed to by the specified lens.

put :: S.MonadState s m => s :-> b -> b -> m ()
put l = S.modify . L.set l

-- | Alias for `put' that reads like an assignment.

infixr 7 =:
(=:) :: S.MonadState s m => s :-> b -> b -> m ()
(=:) = put

-- | Modify a value with a function somewhere in state pointed to by the
-- specified lens.

modify :: S.MonadState s m => s :-> b -> (b -> b) -> m ()
modify l = S.modify . L.mod l

-- | Fetch a value pointed to by a lens out of a reader environment.

ask :: R.MonadReader r m => (r :-> b) -> m b
ask = R.asks . L.get

-- | Execute a computation in a modified environment. The lens is used to
-- point out the part to modify.

local :: R.MonadReader r m => (r :-> b) -> (b -> b) -> m a -> m a
local l f = R.local (L.mod l f)

{-

-- import Data.Record.Label.Maybe

-- | Get a value out of state pointed to by the specified lens that might fail.
-- When the lens getter fails this computation will fall back to mzero.

getMP :: (MonadState f m, MonadPlus m) => (f :~> b) -> m b
getMP l = (getL l `liftM` get) >>= (mzero `maybe` return)

-- | Fetch a value pointed to by a lens that might fail out of a reader
-- environment. When the lens getter fails this computation will fall back to
-- mzero.

askMP :: (MonadReader f m, MonadPlus m) => (f :~> a) -> m a
askMP l = (getL l `liftM` ask) >>= (mzero `maybe` return)

-}
