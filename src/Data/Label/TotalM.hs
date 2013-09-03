{-| State and Reader operations specialized for working with total lenses. -}

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
import Data.Label.Total ((:->), get, set)
import qualified Data.Label.Total      as Total

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State  as State

-- | Get a value out of the state, pointed to by the specified lens.

gets :: State.MonadState s m => s :-> a -> m a
gets = State.gets . get

-- | Set a value somewhere in the state, pointed to by the specified lens.

puts :: State.MonadState s m => s :-> a -> a -> m ()
puts l = State.modify . set l

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified lens.

modify :: State.MonadState s m => s :-> a -> (a -> a) -> m ()
modify l = State.modify . Total.modify l

-- | Alias for `puts' that reads like an assignment.

infixr 2 =:
(=:) :: State.MonadState s m => s :-> a -> a -> m ()
(=:) = puts

-- | Alias for `modify' that reads more or less like an assignment.

infixr 2 =.
(=.) :: State.MonadState s m => s :-> a -> (a -> a) -> m ()
(=.) = modify

-- | Fetch a value pointed to by a lens out of a reader environment.

asks :: Reader.MonadReader r m => (r :-> a) -> m a
asks = Reader.asks . get

-- | Execute a computation in a modified environment. The lens is used to
-- point out the part to modify.

local :: Reader.MonadReader r m => (r :-> b) -> (b -> b) -> m a -> m a
local l f = Reader.local (Total.modify l f)

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified lens. Additionally return a separate value based on the
-- modification.

modifyAndGet :: State.MonadState s m => (s :-> a) -> (a -> (b, a)) -> m b
modifyAndGet l f =
  do (b, a) <- f `liftM` gets l
     puts l a
     return b

