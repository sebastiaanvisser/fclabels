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

-- * Working with isomorphisms.
, Isomorphism (..)
, inv

-- * Specialized lens contexts.
, Total
, Partial
, Failing

-- * Arrow type class for failing with some error.
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
{-# INLINE inv     #-}
{-# INLINE const   #-}
{-# INLINE curry   #-}

-------------------------------------------------------------------------------

-- | Abstract Point datatype. The getter and modifier functions work in some
-- category.

data Point cat g i f o = Point (cat f o) (cat (cat o i, f) g)

-- | Get the getter arrow from a Point.

get :: Point cat g i f o -> cat f o
get (Point g _) = g

-- | Get the setter arrow from a Point.

set :: Arrow arr => Point arr g i f o -> arr (i, f) g
set p = modify p . first (arr const)

-- | Get the modifier arrow from a Point.

modify :: Point cat g i f o -> cat (cat o i, f) g
modify (Point _ m) = m

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

infix 8 `Iso`

-- | An isomorphism is like a `Category` that works in two directions.

data Isomorphism cat i o = Iso { fw :: cat i o, bw :: cat o i }

-- | Isomorphisms are categories.

instance Category cat => Category (Isomorphism cat) where
  id = Iso Cat.id Cat.id
  Iso a b . Iso c d = Iso (a . c) (d . b)
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Flip an isomorphism.

inv :: Isomorphism cat i o -> Isomorphism cat o i
inv i = Iso (bw i) (fw i)

-------------------------------------------------------------------------------

-- | Context that represents computations that always produce an output.

type Total = (->)

-- | Context that represents computations that might silently fail.

type Partial = Kleisli Maybe

-- | Context that represents computations that might fail with some error.

type Failing e = Kleisli (Either e)

-- | The ArrowFail class embeds some error in a failing arrow computation.

class Arrow a => ArrowFail e a where
  failArrow :: a e c

instance ArrowFail e Partial where
  failArrow = Kleisli (const Nothing)
  {-# INLINE failArrow #-}

instance ArrowFail e (Failing e) where
  failArrow = Kleisli Left
  {-# INLINE failArrow #-}

-------------------------------------------------------------------------------
-- Helpers.

const :: Arrow arr => c -> arr b c
const a = arr (\_ -> a)

curry :: Arrow cat => cat (a, b) c -> a -> cat b c
curry m i = m . (const i &&& Cat.id)

