{-# LANGUAGE
    TypeOperators
  , Arrows
  , TupleSections
  , FlexibleInstances
  , ScopedTypeVariables
  , MultiParamTypeClasses
  #-}
module Data.Record.Label.Abstract where

import Control.Arrow
import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category

-- | Abstract Point datatype. The getter and setter functions may work in
-- 'some' arrow.

data Point (~>) f i o = Point
  { _get :: f ~> o
  , _set :: (i, f) ~> f
  }

-- | Modification as a compositon of a getter and setter. Unfortunately,
-- ArrowApply is needed for this composition.

_mod :: ArrowApply (~>) => Point (~>) f i o -> (o ~> i, f) ~> f
_mod l = proc (m, f) -> do i <- m . _get l -<< f; _set l -< (i, f)

-- | Abstract Lens datatype. The getter and setter functions may work in
-- 'some' arrow. Arrows allow for efectful lenses, for example, lenses that
-- might fail or use state.

newtype Lens (~>) f a = Lens { unLens :: Point (~>) f a a }

-- | Create a lens out of a getter and setter.

lens :: (f ~> a) -> ((a, f) ~> f) -> Lens (~>) f a
lens g s = Lens (Point g s)

-- | Get the getter arrow from a lens.

getL :: Arrow (~>) => Lens (~>) f a -> f ~> a
getL = _get . unLens

-- | Get the setter arrow from a lens.

setL :: Arrow (~>) => Lens (~>) f a -> (a, f) ~> f
setL = _set . unLens

-- | Get the modifier arrow from a lens.

modL :: ArrowApply (~>) => Lens (~>) f o -> (o ~> o, f) ~> f
modL = _mod . unLens

instance ArrowApply (~>) => Category (Lens (~>)) where
  id = lens id (arr snd)
  Lens a . Lens b = lens (_get a . _get b) (_mod b . first (curryA (_set a)))
    where curryA f = arr (\i -> f . arr (i,))

instance Arrow (~>) => Functor (Point (~>) f i) where
  fmap f x = Point (arr f . _get x) (_set x)

instance Arrow (~>) => Applicative (Point (~>) f i) where
  pure a  = Point (arr (const a)) (arr snd)
  a <*> b = Point (arr app . (_get a &&& _get b)) (_set b . (arr fst &&& _set a))

-- | Make a 'Point' divergence in two directions.

bimap :: Arrow (~>) => (o' ~> o) -> (i ~> i') -> Point (~>) f i' o' -> Point (~>) f i o
bimap f g l = Point (f . _get l) (_set l . first g)

for :: Arrow (~>) => (i ~> o) -> Lens (~>) f o -> Point (~>) f i o
for a b = bimap id a (unLens b)

-- | The bijections datatype, an arrow that works in two directions. 

data Bijection (~>) a b = Bij { fw :: a ~> b, bw :: b ~> a }

-- | Bijections as categories.

instance Category (~>) => Category (Bijection (~>)) where
  id = Bij id id
  Bij a b . Bij c d = Bij (a . c) (d . b)

-- | Lifting 'Bijection's.

liftBij :: Functor f => Bijection (->) a b -> Bijection (->) (f a) (f b)
liftBij a = Bij (fmap (fw a)) (fmap (bw a))

-- | The isomorphism type class is like a `Functor' but works in two directions.

class Iso (~>) f where
  iso :: Bijection (~>) a b -> f a ~> f b

-- | We can diverge 'Lens'es using an isomorphism.

instance Arrow (~>) => Iso (~>) (Lens (~>) f) where
  iso bi = arr ((\a -> lens (fw bi . _get a) (_set a . first (bw bi))) . unLens)

-- | We can diverge 'Bijection's using an isomorphism.

instance Arrow (~>) => Iso (~>) (Bijection (~>) a) where
  iso = arr . (.)
