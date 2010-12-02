{-# LANGUAGE TypeOperators, TypeSynonymInstances, TemplateHaskell #-}
module Data.Record.Label.Core
(
-- * Lens types.
  Point (Point)
, (:->) (Lens)
, lens
, getL, setL, modL

, fmapL

-- * Bidirectional functor.
, (:<->:) (..)
, Iso (..)
, lmap
, for

-- * Derive labels using Template Haskell.
, module Data.Record.Label.TH
)
where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category
import Control.Monad.State
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

getL :: (f :-> a) -> f -> a
getL = _get . unLens

-- | Get the setter function from a lens.

setL :: (f :-> a) -> a -> f -> f
setL = _set . unLens

-- | Get the modifier function from a lens.

modL :: (f :-> a) -> (a -> a) -> f -> f
modL = _mod . unLens

instance Category (:->) where
  id = lens id const
  Lens a . Lens b = lens (_get a . _get b) (_mod b . _set a)

instance Functor (Point f i) where
  fmap f x = Point (f . _get x) (_set x)

instance Applicative (Point f i) where
  pure a = Point (const a) (const id)
  a <*> b = Point (_get a <*> _get b) (\r -> _set b r . _set a r)

fmapL :: Applicative f => (a :-> b) -> f a :-> f b
fmapL l = lens (fmap (getL l)) (\x f -> setL l <$> x <*> f)

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

infixr 8 %

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

