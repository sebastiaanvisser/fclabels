{-| State and Reader operations specialized for working with total lenses. -}

{-# LANGUAGE TypeOperators  #-}

module Data.Label.Monadic
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
import Data.Label.Mono (Lens)

import qualified Data.Label.Total     as Total
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State  as State

-- | Get a value out of the state, pointed to by the specified lens.

gets :: State.MonadState f m => Lens (->) f o -> m o
gets = State.gets . Total.get

-- | Set a value somewhere in the state, pointed to by the specified lens.

puts :: State.MonadState f m => Lens (->) f o -> o -> m ()
puts l = State.modify . Total.set l

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified lens.

modify :: State.MonadState f m => Lens (->) f o -> (o -> o) -> m ()
modify l = State.modify . Total.modify l

-- | Alias for `puts' that reads like an assignment.

infixr 2 =:
(=:) :: State.MonadState f m => Lens (->) f o -> o -> m ()
(=:) = puts

-- | Alias for `modify' that reads more or less like an assignment.

infixr 2 =.
(=.) :: State.MonadState f m => Lens (->) f o -> (o -> o) -> m ()
(=.) = modify

-- | Fetch a value pointed to by a lens out of a reader environment.

asks :: Reader.MonadReader f m => (Lens (->) f o) -> m o
asks = Reader.asks . Total.get

-- | Execute a computation in a modified environment. The lens is used to
-- point out the part to modify.

local :: Reader.MonadReader f m => (Lens (->) f o) -> (o -> o) -> m a -> m a
local l f = Reader.local (Total.modify l f)

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified lens. Additionally return a separate value based on the
-- modification.

modifyAndGet :: State.MonadState f m => (Lens (->) f o) -> (o -> (a, o)) -> m a
modifyAndGet l f =
  do (b, a) <- f `liftM` gets l
     puts l a
     return b

