{-# LANGUAGE TypeOperators, TypeSynonymInstances, TemplateHaskell #-}
module Data.Record.Label
  (
  -- * Lens types.
    Point (Point)
  , (:->) (Lens)
  , lens
  , get, set, mod

  , fmapL

  -- * Bidirectional functor.
  , (:<->:) (..)
  , Iso (..)
  , lmap
  , for

  -- * Monadic lens operations.
  , getM, setM, modM, (=:)
  , askM, localM

  -- * Derive labels using Template Haskell.
  , module Data.Record.Label.TH
  )
where

import Prelude hiding ((.), id, mod)
import Control.Applicative
import Control.Category
import Control.Monad.State hiding (get)
import Control.Monad.Reader
import Data.Record.Label.TH

data Point f i o = Point
  { _get :: f -> o
  , _set :: i -> f -> f
  }

_mod :: Point f i o -> (o -> i) -> f -> f
_mod l f a = _set l (f (_get l a)) a

newtype (f :-> a) = Lens { unLens :: Point f a a }

-- | Create a lens out of a getter and setter.

lens :: (f -> a) -> (a -> f -> f) -> f :-> a
lens g s = Lens (Point g s)

-- | Get the getter function from a lens.

get :: (f :-> a) -> f -> a
get = _get . unLens

-- | Get the setter function from a lens.

set :: (f :-> a) -> a -> f -> f
set = _set . unLens

-- | Get the modifier function from a lens.

mod :: (f :-> a) -> (a -> a) -> f -> f
mod = _mod . unLens

instance Category (:->) where
  id = lens id const
  Lens a . Lens b = lens (_get a . _get b) (_mod b . _set a)

instance Functor (Point f i) where
  fmap f x = Point (f . _get x) (_set x)

instance Applicative (Point f i) where
  pure a = Point (const a) (const id)
  a <*> b = Point (_get a <*> _get b) (\r -> _set b r . _set a r)

fmapL :: Applicative f => (a :-> b) -> f a :-> f b
fmapL l = lens (fmap (get l)) (\x f -> set l <$> x <*> f)

-- | This isomorphism type class is like a `Functor' but works in two directions.

class Iso f where
  (%) :: a :<->: b -> f a -> f b

-- | The bijections datatype, a function that works in two directions. 

infixr 7 :<->:
data a :<->: b = (:<->:) { fw :: a -> b, bw :: b -> a }

-- | Constructor for bijections.

instance Category (:<->:) where
  id = id :<->: id
  (a :<->: b) . (c :<->: d) = a . c :<->: d . b

instance Iso ((:->) i) where
  l % Lens a = lens (fw l . _get a) (_set a . bw l)

instance Iso ((:<->:) i) where
  (%) = (.)

lmap :: Functor f => (a :<->: b) -> f a :<->: f b 
lmap l = let a :<->: b = l in fmap a :<->: fmap b

dimap :: (o' -> o) -> (i -> i') -> Point f i' o' -> Point f i o
dimap f g l = Point (f . _get l) (_set l . g)

-- | Combine a partial destructor with a lens into something easily used in the
-- applicative instance for the hidden `Point' datatype. Internally uses the
-- covariant in getter, contravariant in setter bi-functioral-map function.
-- (Please refer to the example because this function is just not explainable
-- on its own.)

for :: (i -> o) -> (f :-> o) -> Point f i o
for a b = dimap id a (unLens b)

-- | Get a value out of state pointed to by the specified lens.

getM :: MonadState s m => s :-> b -> m b
getM = gets . get

-- | Set a value somewhere in state pointed to by the specified lens.

setM :: MonadState s m => s :-> b -> b -> m ()
setM l = modify . set l

-- | Alias for `setM' that reads like an assignment.

infixr 7 =:
(=:) :: MonadState s m => s :-> b -> b -> m ()
(=:) = setM

-- | Modify a value with a function somewhere in state pointed to by the
-- specified lens.

modM :: MonadState s m => s :-> b -> (b -> b) -> m ()
modM l = modify . mod l

-- | Fetch a value pointed to by a lens out of a reader environment.

askM :: MonadReader r m => (r :-> b) -> m b
askM = asks . get

-- | Execute a computation in a modified environment. The lens is used to
-- point out the part to modify.

localM :: MonadReader r m => (r :-> b) -> (b -> b) -> m a -> m a
localM l f = local (mod l f)

