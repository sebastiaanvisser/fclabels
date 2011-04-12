{-# LANGUAGE TypeOperators  #-}
module Data.Label.Monadic
(
-- * 'MonadState' lens operations.
  get
, put
, modify
, (=:)

-- * 'MonadReader' lens operations.
, ask
, local
)
where

import Data.Label.Pure ((:->))
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State  as M
import qualified Data.Label.Pure      as L

-- | Get a value out of the state, pointed to by the specified lens.

get :: M.MonadState s m => s :-> b -> m b
get = M.gets . L.get

-- | Set a value somewhere in the state, pointed to by the specified lens.

put :: M.MonadState s m => s :-> b -> b -> m ()
put l = M.modify . L.set l

-- | Alias for `put' that reads like an assignment.

infixr 7 =:
(=:) :: M.MonadState s m => s :-> b -> b -> m ()
(=:) = put

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified lens.

modify :: M.MonadState s m => s :-> b -> (b -> b) -> m ()
modify l = M.modify . L.mod l

-- | Fetch a value pointed to by a lens out of a reader environment.

ask :: M.MonadReader r m => (r :-> b) -> m b
ask = M.asks . L.get

-- | Execute a computation in a modified environment. The lens is used to
-- point out the part to modify.

local :: M.MonadReader r m => (r :-> b) -> (b -> b) -> m a -> m a
local l f = M.local (L.mod l f)

