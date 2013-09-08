{- | Lenses that only allow monomorphic updates. -}

{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses #-}

module Data.Label.Mono
( Lens (..)
, lens
, get
, set
, modify
, for
)
where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id, const, curry)
import Data.Label.Point (Point (Point), Iso, Bijection (Bij))

import qualified Data.Label.Point as Point

{-# INLINE lens   #-}
{-# INLINE get    #-}
{-# INLINE set    #-}
{-# INLINE modify #-}
{-# INLINE for    #-}

-------------------------------------------------------------------------------

-- | Abstract monomorphic lens datatype. The getter and setter functions work
-- in some category. Categories allow for effectful lenses, for example, lenses
-- that might fail or use state.

newtype Lens cat f a = Lens { point :: Point cat f a f a }

-- | Create a lens out of a getter and setter.

lens :: cat f a -> (cat (cat a a, f) f) -> Lens cat f a
lens g m = Lens (Point g m)

-- | Get the getter arrow from a lens.

get :: Lens cat f a -> cat f a
get = Point.get . point

-- | Get the setter arrow from a lens.

set :: Arrow arr => Lens arr f a -> arr (a, f) f
set = Point.set . point

-- | Get the modifier arrow from a lens.

modify :: Lens cat f a -> cat (cat a a, f) f
modify = Point.modify . point

-------------------------------------------------------------------------------

-- | Category instance for monomorphic lenses.

instance ArrowApply arr => Category (Lens arr) where
  id              = Lens Point.id
  Lens a . Lens b = Lens (Point.compose a b)
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

infix 8 `for`

-- | Make a Lens output diverge by changing the input of the modifier.

for :: Arrow arr => arr i o -> Lens arr f o -> Point arr f i f o
for f (Lens l) = Point (Point.get l) (Point.modify l . first (arr (f .)))

-- | We can diverge 'Lens'es using an isomorphism.

instance Arrow arr => Iso arr (Lens arr f) where
  iso (Bij f b) (Lens (Point g m)) =
    lens (f . g) (m . first (arr (\a -> b . a . f)))
  {-# INLINE iso #-}

