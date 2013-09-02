{-# LANGUAGE TypeOperators  #-}
module Data.Label.TotalM
(
-- * 'MonadState' lens operations.
  gets
, puts
, modify
, modifyAndGet
, (=:)
, (=.)

-- * 'MonadReader' lens operations.
, asks
, local
)
where

import Control.Monad
import Data.Label.Total ((:->))
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State  as M
import qualified Data.Label.Total      as L

-- | Get a value out of the state, pointed to by the specified lens.

gets :: M.MonadState s m => s :-> a -> m a
gets = M.gets . L.get

-- | Set a value somewhere in the state, pointed to by the specified lens.

puts :: M.MonadState s m => s :-> a -> a -> m ()
puts l = M.modify . L.set l

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified lens.

modify :: M.MonadState s m => s :-> a -> (a -> a) -> m ()
modify l = M.modify . L.modify l

-- | Alias for `puts' that reads like an assignment.

infixr 2 =:
(=:) :: M.MonadState s m => s :-> a -> a -> m ()
(=:) = puts

-- | Alias for `modify' that reads more or less like an assignment.

infixr 2 =.
(=.) :: M.MonadState s m => s :-> a -> (a -> a) -> m ()
(=.) = modify

-- | Fetch a value pointed to by a lens out of a reader environment.

asks :: M.MonadReader r m => (r :-> a) -> m a
asks = M.asks . L.get

-- | Execute a computation in a modified environment. The lens is used to
-- point out the part to modify.

local :: M.MonadReader r m => (r :-> b) -> (b -> b) -> m a -> m a
local l f = M.local (L.modify l f)

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified lens. Additionally return a separate value based on the
-- modification.

modifyAndGet :: M.MonadState s m => (s :-> a) -> (a -> (b, a)) -> m b
modifyAndGet l f =
  do (b, a) <- f `liftM` gets l
     puts l a
     return b

