{-# LANGUAGE
    TypeOperators
  , Arrows
  , TupleSections
  , FlexibleInstances
  , MultiParamTypeClasses #-}
module Data.Label.Abstract where

import Control.Arrow
import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category

{-# INLINE _modify #-}
{-# INLINE lens    #-}
{-# INLINE get     #-}
{-# INLINE set     #-}
{-# INLINE modify  #-}
{-# INLINE bimap   #-}
{-# INLINE for     #-}
{-# INLINE liftBij #-}

-- | Abstract Point datatype. The getter and setter functions work in some
-- arrow.

data Point arr f i o = Point
  { _get :: f `arr` o
  , _set :: (i, f) `arr` f
  }

-- | Modification as a compositon of a getter and setter. Unfortunately,
-- `ArrowApply' is needed for this composition.

_modify :: ArrowApply arr => Point arr f i o -> (o `arr` i, f) `arr` f
_modify l = proc (m, f) -> do i <- m . _get l -<< f; _set l -< (i, f)

-- | Abstract Lens datatype. The getter and setter functions work in some
-- arrow. Arrows allow for effectful lenses, for example, lenses that might
-- fail or use state.

newtype Lens arr f a = Lens { unLens :: Point arr f a a }

-- | Create a lens out of a getter and setter.

lens :: (f `arr` a) -> ((a, f) `arr` f) -> Lens arr f a
lens g s = Lens (Point g s)

-- | Get the getter arrow from a lens.

get :: Arrow arr => Lens arr f a -> f `arr` a
get = _get . unLens

-- | Get the setter arrow from a lens.

set :: Arrow arr => Lens arr f a -> (a, f) `arr` f
set = _set . unLens

-- | Get the modifier arrow from a lens.

modify :: ArrowApply arr => Lens arr f o -> (o `arr` o, f) `arr` f
modify = _modify . unLens

instance ArrowApply arr => Category (Lens arr) where
  id = lens id (arr fst)
  Lens a . Lens b = lens (_get a . _get b) (_modify b . first (curryA (_set a)))
    where curryA f = arr (\i -> f . arr (i,))
  {-# INLINE id #-}
  {-# INLINE (.) #-}

instance Arrow arr => Functor (Point arr f i) where
  fmap f x = Point (arr f . _get x) (_set x)
  {-# INLINE fmap #-}

instance Arrow arr => Applicative (Point arr f i) where
  pure a  = Point (arr (const a)) (arr snd)
  a <*> b = Point (arr app . (_get a &&& _get b)) (_set b . (arr fst &&& _set a))
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

-- | Make a 'Point' diverge in two directions.

bimap :: Arrow arr => (o' `arr` o) -> (i `arr` i') -> Point arr f i' o' -> Point arr f i o
bimap f g l = Point (f . _get l) (_set l . first g)

infix 8 `for`

for :: Arrow arr => (i `arr` o) -> Lens arr f o -> Point arr f i o
for p = bimap id p . unLens

-- | The bijections datatype, an arrow that works in two directions. 

infix 8 `Bij`

data Bijection arr a b = Bij { fw :: a `arr` b, bw :: b `arr` a }

-- | Bijections as categories.

instance Category arr => Category (Bijection arr) where
  id = Bij id id
  Bij a b . Bij c d = a . c `Bij` d . b
  {-# INLINE id #-}
  {-# INLINE (.) #-}

-- | Lifting 'Bijection's.

liftBij :: Functor f => Bijection (->) a b -> Bijection (->) (f a) (f b)
liftBij a = fmap (fw a) `Bij` fmap (bw a)

-- | The isomorphism type class is like a `Functor' can work in two directions.

infixr 8 `iso`

class Iso arr f where
  iso :: Bijection arr a b -> f a -> f b

-- | Flip an isomorphism.

inv :: Bijection arr b a -> Bijection arr a b
inv (Bij a b) = (Bij b a)

-- | We can diverge 'Lens'es using an isomorphism.

instance Arrow arr => Iso arr (Lens arr f) where
  iso bi (Lens l) = lens (fw bi . _get l) (_set l . first (bw bi))
  {-# INLINE iso #-}

-- | We can diverge 'Bijection's using an isomorphism.

instance Arrow arr => Iso arr (Bijection arr a) where
  iso = (.)
  {-# INLINE iso #-}

