{-# LANGUAGE TypeOperators, TypeSynonymInstances, TemplateHaskell #-}
module Data.Record.Label
  (
  -- * Getter, setter and modifier types.
    Getter
  , Setter
  , Modifier

  -- * Label type.
  , Point
  , (:->) (Label)
  , label
  , get, set, mod

  -- * Bidirectional functor.
  , (:<->:) (..)
  , (<->)
  , Iso (..)
  , for

  -- * State monadic label operations.

  , getM, setM, modM, (=:)

  -- * Derive labels using Template Haskell.
  , module Data.Record.Label.TH
  )
where

import Prelude hiding ((.), id, mod)
import Control.Applicative
import Control.Category
import Control.Monad.State hiding (get)
import Data.Record.Label.TH

type Getter   f o   = f -> o
type Setter   f i   = i -> f -> f
type Modifier f i o = (o -> i) -> f -> f

data Point f i o = Point
  { _get :: Getter f o
  , _set :: Setter f i
  }

_mod :: Point f i o -> (o -> i) -> f -> f
_mod l f a = _set l (f (_get l a)) a

newtype (f :-> a) = Label { unLabel :: Point f a a }

-- Create a label out of a getter and setter.

label :: Getter f a -> Setter f a -> f :-> a
label g s = Label (Point g s)

-- | Get the getter function from a label.

get :: (f :-> a) -> f -> a
get = _get . unLabel

-- | Get the setter function from a label.

set :: (f :-> a) -> a -> f -> f
set = _set . unLabel

-- | Get the modifier function from a label.

mod :: (f :-> a) -> (a -> a) -> f -> f
mod = _mod . unLabel

instance Category (:->) where
  id = Label (Point id const)
  (Label a) . (Label b) = Label (Point (_get a . _get b) (_mod b . _set a))

instance Functor (Point f i) where
  fmap f x = Point (f . _get x) (_set x)

instance Applicative (Point f i) where
  pure a = Point (const a) (const id)
  a <*> b = Point (\f -> _get a f (_get b f)) (\r -> _set b r . _set a r)

-- | This isomorphism type class is like a `Functor' but works in two directions.

class Iso f where
  iso :: a :<->: b -> f a -> f b
  iso (Lens a b) = osi (b <-> a)
  osi :: a :<->: b -> f b -> f a
  osi (Lens a b) = iso (b <-> a)

-- | The lens datatype, a function that works in two directions. To bad there
-- is no convenient way to do application for this.

data a :<->: b = Lens { fw :: a -> b, bw :: b -> a }

-- | Constructor for lenses.

infixr 7 <->
(<->) :: (a -> b) -> (b -> a) -> a :<->: b
a <-> b = Lens a b

instance Category (:<->:) where
  id = Lens id id
  (Lens a b) . (Lens c d) = Lens (a . c) (d . b)

instance Iso ((:->) i) where
  iso l (Label a) = Label (Point (fw l . _get a) (_set a . bw l))

instance Iso ((:<->:) i) where
  iso = (.)

dimap :: (o' -> o) -> (i -> i') -> Point f i' o' -> Point f i o
dimap f g l = Point (f . _get l) (_set l . g)

-- | Combine a partial destructor with a label into something easily used in
-- the applicative instance for the hidden `Point' datatype. Internally uses
-- the covariant in getter, contravariant in setter bi-functioral-map function.
-- (Please refer to the example because this function is just not explainable
-- on its own.)

for :: (i -> o) -> (f :-> o) -> Point f i o
for a b = dimap id a (unLabel b)

-- | Get a value out of state pointed to by the specified label.

getM :: MonadState s m => s :-> b -> m b
getM = gets . get

-- | Set a value somewhere in state pointed to by the specified label.

setM :: MonadState s m => s :-> b -> b -> m ()
setM l = modify . set l

-- | Alias for `setM' that reads like an assignment.

infixr 7 =:
(=:) :: MonadState s m => s :-> b -> b -> m ()
(=:) = setM

-- | Modify a value with a function somewhere in state pointed to by the
-- specified label.

modM :: MonadState s m => s :-> b -> (b -> b) -> m ()
modM l = modify . mod l

