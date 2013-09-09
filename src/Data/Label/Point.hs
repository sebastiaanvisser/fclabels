{- | The Point data type which generalizes the different lenses and forms the
basis for vertical composition using the `Applicative` type class.
-}

{-# LANGUAGE
    TypeOperators
  , Arrows
  , FlexibleInstances
  , MultiParamTypeClasses #-}

module Data.Label.Point
(
-- * The point data type that generalizes lens.
  Point (Point)
, modify
, get
, set
, id
, compose

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
)
where

import Control.Arrow
import Control.Applicative
import Control.Category hiding (id)
import Prelude hiding ((.), id, const, curry)

import qualified Control.Category as Cat

{-# INLINE get     #-}
{-# INLINE modify  #-}
{-# INLINE set     #-}
{-# INLINE id      #-}
{-# INLINE compose #-}
{-# INLINE liftBij #-}
{-# INLINE inv     #-}
{-# INLINE const   #-}
{-# INLINE curry   #-}

-------------------------------------------------------------------------------

-- | Abstract Point datatype. The getter and modifier functions work in some
-- category.

data Point cat g i f o = Point (cat f o) (cat (cat o i, f) g)

-- | Get a value.

get :: Point cat g i f o -> cat f o
get (Point g _) = g

-- | Modify a value.

modify :: Point cat g i f o -> cat (cat o i, f) g
modify (Point _ m) = m

-- | Setting a value in terms of modification with the constant function.

set :: Arrow arr => Point arr g i f o -> arr (i, f) g
set p = modify p . first (arr const)

-- | Identity `Point`.

id :: ArrowApply arr => Point arr f f o o
id = Point Cat.id app

-- | `Point` composition.

compose :: ArrowApply cat
        => Point cat t i b o
        -> Point cat g t f b
        -> Point cat g i f o
compose (Point gi mi) (Point go mo)
  = Point (gi <<< go) (app <<< arr (first (curry mo <<< curry mi)))

-------------------------------------------------------------------------------

instance Arrow arr => Functor (Point arr f i f) where
  fmap f x = pure f <*> x
  {-# INLINE fmap #-}

instance Arrow arr => Applicative (Point arr f i f) where
  pure a  = Point (const a) (arr snd)
  a <*> b = Point (arr app . (get a &&& get b)) $
    proc (t, p) -> do (f, v) <- get a &&& get b -< p
                      q <- modify a             -< (t . arr ($ v), p)
                      modify b                  -< (t . arr f, q)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

-------------------------------------------------------------------------------

infix 8 `Bij`

-- | The bijections datatype, a category that works in two directions.

data Bijection cat i o = Bij { fw :: cat i o, bw :: cat o i }

-- | Bijections as categories.

instance Category cat => Category (Bijection cat) where
  id = Bij Cat.id Cat.id
  Bij a b . Bij c d = Bij (a . c) (d . b)
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Lifting 'Bijection's.

liftBij :: Functor f => Bijection (->) i o -> Bijection (->) (f i) (f o)
liftBij (Bij f b) = fmap f `Bij` fmap b

infixr 8 `iso`

-- | The isomorphism type class is like a `Functor' can work in two directions.

class Iso cat f where
  iso :: Bijection cat i o -> f i -> f o

-- | Flip an isomorphism.

inv :: Bijection cat i o -> Bijection cat o i
inv (Bij a b) = (Bij b a)

-- | We can diverge 'Bijection's using an isomorphism.

instance Arrow arr => Iso arr (Bijection arr i) where
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

curry :: Arrow cat => cat (a, b) c -> a -> cat b c
curry m i = m . (const i &&& Cat.id)

