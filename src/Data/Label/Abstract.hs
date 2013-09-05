{- | The lens internals. -}

{-# LANGUAGE
    TypeOperators
  , Arrows
  , FlexibleInstances
  , MultiParamTypeClasses #-}

module Data.Label.Abstract
(
-- * The point data type that generalizes lens.
   Point (Point)
, _modify
, _get
, _set

-- * A lens working in some abstract context.
, Lens (..)
, lens
, get
, set
, modify

, for

-- * Working with isomorphisms and bijections.
, Iso (..)
, inv
, Bijection (..)
, liftBij

-- * Specialized lens contexts.
, Total
, Partial
, Failing

-- * Missing Arrow type class for failing with some error.
, ArrowFail (..)

-- * Helpers
, pack
)
where

import Control.Arrow
import Control.Applicative
import Control.Category
import Prelude hiding ((.), id, const)

{-# INLINE _get    #-}
{-# INLINE _modify #-}
{-# INLINE _set    #-}
{-# INLINE lens    #-}
{-# INLINE get     #-}
{-# INLINE set     #-}
{-# INLINE modify  #-}
{-# INLINE compose #-}
{-# INLINE for     #-}
{-# INLINE liftBij #-}
{-# INLINE inv     #-}
{-# INLINE pack    #-}
{-# INLINE const   #-}

-------------------------------------------------------------------------------

-- | Abstract Point datatype. The getter and modifier functions work in some
-- category.

data Point cat g i f o = Point (cat f o) (cat (cat o i, f) g)

-- | Get a value.

_get :: Point cat g i f o -> cat f o
_get (Point g _) = g

-- | Modify a value.

_modify :: Point cat g i f o -> cat (cat o i, f) g
_modify (Point _ m) = m

-- | Setting a value in terms of modification with the constant function.

_set :: Arrow arr => Point arr g i f o -> arr (i, f) g
_set p = _modify p . first (arr const)

-------------------------------------------------------------------------------

-- | Abstract Lens datatype. The getter and setter functions work in some
-- category. Categories allow for effectful lenses, for example, lenses that
-- might fail or use state.

newtype Lens cat f a = Lens { unLens :: Point cat f a f a }

-- | Create a lens out of a getter and setter.

lens :: cat f a -> (cat (cat a a, f) f) -> Lens cat f a
lens g m = Lens (Point g m)

-- | Get the getter arrow from a lens.

get :: Arrow arr => Lens arr f a -> arr f a
get = _get . unLens

-- | Get the setter arrow from a lens.

set :: Arrow arr => Lens arr f a -> arr (a, f) f
set = _set . unLens

-- | Get the modifier arrow from a lens.

modify :: ArrowApply arr => Lens arr f a -> arr (arr a a, f) f
modify = _modify . unLens

compose :: ArrowApply arr => Point arr f i o -> Point arr g f f -> Point arr g i o
compose (Point gi mi) (Point go mo) =
  Point (gi . go) (app . arr (first (\oi -> mo . pack (mi . pack oi))))

-------------------------------------------------------------------------------

instance ArrowApply arr => Category (Lens arr) where
  id = lens id app
  Lens (Point gi mi) . Lens (Point go mo) = Lens $
    Point (gi . go) (app . arr (first (\a -> mo . pack (mi . pack a))))
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

instance Arrow arr => Functor (Point arr f i f) where
  fmap f x = pure f <*> x
  {-# INLINE fmap #-}

instance Arrow arr => Applicative (Point arr f i f) where
  pure a  = Point (const a) (arr snd)
  a <*> b = Point (arr app . (_get a &&& _get b)) $
    proc (t, p) -> do (f, v) <- _get a &&& _get b -< p
                      q <- _modify a              -< (t . arr ($ v), p)
                      _modify b                   -< (t . arr f, q)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

infix 8 `for`

-- | Make a Lens output diverge by modification of the setter input.

for :: Arrow arr => arr i o -> Lens arr f o -> Point arr f i f o
for f (Lens l) = Point (_get l) (_modify l . first (arr (f .)))

-------------------------------------------------------------------------------

infix 8 `Bij`

-- | The bijections datatype, a category that works in two directions.

data Bijection cat a b = Bij { fw :: cat a b, bw :: cat b a }

-- | Bijections as categories.

instance Category cat => Category (Bijection cat) where
  id = Bij id id
  Bij a b . Bij c d = Bij (a . c) (d . b)
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Lifting 'Bijection's.

liftBij :: Functor f => Bijection (->) a b -> Bijection (->) (f a) (f b)
liftBij (Bij f b) = fmap f `Bij` fmap b

infixr 8 `iso`

-- | The isomorphism type class is like a `Functor' can work in two directions.

class Iso cat f where
  iso :: Bijection cat a b -> f a -> f b

-- | Flip an isomorphism.

inv :: Bijection cat b a -> Bijection cat a b
inv (Bij a b) = (Bij b a)

-- | We can diverge 'Lens'es using an isomorphism.

instance Arrow arr => Iso arr (Lens arr f) where
  iso (Bij f b) (Lens (Point g m)) =
    lens (f . g) (m . first (arr (\a -> b . a . f)))
  {-# INLINE iso #-}

-- | We can diverge 'Bijection's using an isomorphism.

instance Arrow arr => Iso arr (Bijection arr a) where
  iso = (.)
  {-# INLINE iso #-}

-------------------------------------------------------------------------------

-- | Context that represents computations that always produce an output.

type Total = (->)

-- | Context that represents computations that might silently fail.

type Partial = Kleisli Maybe

-- | Context that represents computations that might fail with some error.

type Failing e = Kleisli (Either e)

-- | The ArrowFail class embed some error in a failing arrow computation.

class Arrow a => ArrowFail e a where
  failArrow :: a e c

instance ArrowFail e Partial where
  failArrow = Kleisli (const Nothing)
  {-# INLINE failArrow #-}

instance ArrowFail e (Failing e) where
  failArrow = Kleisli Left
  {-# INLINE failArrow #-}

-------------------------------------------------------------------------------

const :: Arrow arr => c -> arr b c
const a = arr (\_ -> a)

pack :: Arrow arr => a -> arr b (a, b)
pack i = const i &&& id

