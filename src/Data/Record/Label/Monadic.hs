{-# LANGUAGE TypeOperators, TypeSynonymInstances, TemplateHaskell #-}
module Data.Record.Label.Monadic
(
-- * Monadic lens operations.
  getM, setM, modM, (=:)
, askM, localM
)
where

import Control.Monad.State
import Control.Monad.Reader
import Data.Record.Label.Pure

-- | Get a value out of state pointed to by the specified lens. Lenses
-- supporting failure will call `fail` in the inner Monad:

getM :: MonadState s m => (s :~> b) -> m b
getM l = join $ gets (liftMaybe . getLM l)

-- | Set a value somewhere in state pointed to by the specified lens.

setM :: MonadState s m => (s :~> b) -> b -> m ()
setM l = modM l . const

-- | Alias for `setM' that reads like an assignment.

infixr 7 =:
(=:) :: MonadState s m => (s :~> b) -> b -> m ()
(=:) = setM

-- | Modify a value with a function somewhere in state pointed to by the
-- specified lens.

modM :: MonadState s m => (s :~> b) -> (b -> b) -> m ()
modM l f = get >>= liftMaybe . modLM l f >>= put

-- | Fetch a value pointed to by a lens out of a reader environment.

askM :: MonadReader r m => (r :~> b) -> m b
askM l = join $ asks (liftMaybe . getLM l)

-- | Execute a computation in a modified environment. The lens is used to
-- point out the part to modify.

localM :: MonadReader r m => (r :~> b) -> (b -> b) -> m a -> m a
localM l f m = ask >>= liftMaybe . modLM l f >>= \r-> local (const r) m

{-
 -      THE COUSIN OF `fmapL` SUPPORTING FAILURE HANDLING. 
 -      ARE WE EXPORTING `fmapL`?
 -
-- Lift a lens into the Monad class. Any failures raised by lenses using 
-- the MaybePoint constructor will call the monad's 'fail' method.
liftML :: Monad m => (a :~> b) -> m a :~> m b
liftML l = undefined
-}

--- lift error-catching in Maybe into Monad, using `return` and `fail`
liftMaybe :: (Monad m)=> Maybe a -> m a
liftMaybe = maybe (fail "Lens failed.") return
