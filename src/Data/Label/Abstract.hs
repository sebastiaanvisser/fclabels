{- | Lens internals. -}
{-# LANGUAGE
    TypeOperators
  , Arrows
  , FlexibleInstances
  , MultiParamTypeClasses #-}
module Data.Label.Abstract where

import Control.Arrow
import Control.Applicative
import Control.Category
import Prelude hiding ((.), id, const)

{-# INLINE _set    #-}
{-# INLINE lens    #-}
{-# INLINE get     #-}
{-# INLINE set     #-}
{-# INLINE modify  #-}
{-# INLINE compose #-}
{-# INLINE for     #-}
{-# INLINE liftBij #-}
{-# INLINE pack    #-}
{-# INLINE const   #-}

-- | Abstract Point datatype. The getter and modifier functions work in some
-- category.

data Point arr f i o = Point
  { _get    :: f `arr` o
  , _modify :: (o `arr` i, f) `arr` f
  }

-- | Setting a value in terms of modification with the constant function.

_set :: Arrow arr => Point arr f i o -> (i, f) `arr` f
_set p = _modify p . first (arr const)

-- | Abstract Lens datatype. The getter and setter functions work in some
-- arrow. Arrows allow for effectful lenses, for example, lenses that might
-- fail or use state.

newtype Lens arr f a = Lens { unLens :: Point arr f a a }

-- | Create a lens out of a getter and setter.

lens :: (f `arr` a) -> ((a `arr` a, f) `arr` f) -> Lens arr f a
lens g m = Lens (Point g m)

-- | Get the getter arrow from a lens.

get :: Arrow arr => Lens arr f a -> f `arr` a
get = _get . unLens

-- | Get the setter arrow from a lens.

set :: Arrow arr => Lens arr f a -> (a, f) `arr` f
set = _set . unLens

-- | Get the modifier arrow from a lens.

modify :: ArrowApply arr => Lens arr f a -> (a `arr` a, f) `arr` f
modify = _modify . unLens

compose :: ArrowApply arr => Point arr f i o -> Point arr g f f -> Point arr g i o
compose (Point gi mi) (Point go mo) =
  Point (gi . go) (app . arr (first (\oi -> mo . pack (mi . pack oi))))

-------------------------------------------------------------------------------

instance ArrowApply arr => Category (Lens arr) where
  id = lens id (arr snd)
  Lens (Point gi mi) . Lens (Point go mo) = Lens $
    Point (gi . go) (app . arr (first (\oi -> mo . pack (mi . pack oi))))
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

instance Arrow arr => Functor (Point arr f i) where
  fmap f x = Point (arr f . _get x) (_modify x . first (arr ((\a -> a . arr f))))
  {-# INLINE fmap #-}

instance Arrow arr => Applicative (Point arr f i) where
  pure a  = Point (const a) (arr snd)
  a <*> b = Point (arr app . (_get a &&& _get b)) $
    proc (t, p) -> do (f, v) <- (_get a &&& _get b) -< p
                      q <- _modify a -< (t . arr ($ v), p)
                      _modify b -< (t . arr f, q)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

infix 8 `for`

for :: Arrow arr => (i `arr` o) -> Lens arr f o -> Point arr f i o
for f (Lens l) = Point (_get l) (_modify l . first (arr (\a -> f . a)))

-------------------------------------------------------------------------------

infix 8 `Bij`

-- | The bijections datatype, an arrow that works in two directions. 

data Bijection arr a b = Bij { fw :: a `arr` b, bw :: b `arr` a }

-- | Bijections as categories.

instance Category arr => Category (Bijection arr) where
  id = Bij id id
  Bij a b . Bij c d = a . c `Bij` d . b
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Lifting 'Bijection's.

liftBij :: Functor f => Bijection (->) a b -> Bijection (->) (f a) (f b)
liftBij a = fmap (fw a) `Bij` fmap (bw a)

infixr 8 `iso`

-- | The isomorphism type class is like a `Functor' can work in two directions.

class Iso arr f where
  iso :: Bijection arr a b -> f a -> f b

-- | Flip an isomorphism.

inv :: Bijection arr b a -> Bijection arr a b
inv (Bij a b) = (Bij b a)

-- | We can diverge 'Lens'es using an isomorphism.

instance Arrow arr => Iso arr (Lens arr f) where
  iso bi = arr $ \(Lens (Point g m)) ->
             lens (fw bi . g)
                  (m . first (arr (\a -> bw bi . a . fw bi)))
  {-# INLINE iso #-}

-- | We can diverge 'Bijection's using an isomorphism.

instance Arrow arr => Iso arr (Bijection arr a) where
  iso = (.)
  {-# INLINE iso #-}

-------------------------------------------------------------------------------

-- | Context that represents computations that always produce an output.

type Total = (->)

-- | Context that represents computations that might fail.

type Partial = Kleisli Maybe

-- | Context that represents computations that might fail.

type Failure e = Kleisli (Either e)

-- | The ArrowFail class embed some error in a failing computation.

class ArrowFail e a where
  failArrow :: a e c

instance ArrowFail e Partial where
  failArrow = Kleisli (const Nothing)
  {-# INLINE failArrow #-}

instance ArrowFail e (Failure e) where
  failArrow = Kleisli Left
  {-# INLINE failArrow #-}

-------------------------------------------------------------------------------

const :: Arrow arr => c -> b `arr` c
const a = arr (\_ -> a)

pack :: Arrow arr => a -> b `arr` (a, b)
pack i = const i &&& id

