{- | Lenses that allow polymorphic updates. -}

{-# LANGUAGE
    FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , TypeOperators #-}

module Data.Label.Poly
(

-- * The polymorphic Lens type.
  Lens
, lens
, point
, get
, modify
, set
, iso
, (>-)
)
where

import Control.Category
import Control.Arrow
import Prelude ()
import Data.Label.Point (Point (Point), Iso(..), identity, compose)

import qualified Data.Label.Point as Point

{-# INLINE lens   #-}
{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}
{-# INLINE (>-)   #-}
{-# INLINE point  #-}
{-# INLINE unpack #-}

-------------------------------------------------------------------------------

-- | Abstract polymorphic lens datatype. The getter and setter functions work
-- in some category. Categories allow for effectful lenses, for example, lenses
-- that might fail or use state.

data Lens cat f o where
  Lens :: !(Point cat g i f o) -> Lens cat (f -> g) (o -> i)
  Id   :: ArrowApply cat => Lens cat f f

-- | Create a lens out of a getter and setter.

lens :: cat f o             -- ^ Getter.
     -> cat (cat o i, f) g  -- ^ Modifier.
     -> Lens cat (f -> g) (o -> i)
lens g m = Lens (Point g m)

-- | Create lens from a `Point`.

point :: Point cat g i f o -> Lens cat (f -> g) (o -> i)
point = Lens

-- | Get the getter arrow from a lens.

get :: Lens cat (f -> g) (o -> i) -> cat f o
get = Point.get . unpack

-- | Get the modifier arrow from a lens.

modify :: Lens cat (f -> g) (o -> i) -> cat (cat o i, f) g
modify = Point.modify . unpack

-- | Get the setter arrow from a lens.

set :: Arrow arr => Lens arr (f -> g) (o -> i) -> arr (i, f) g
set = Point.set . unpack

-- | Lift a polymorphic isomorphism into a `Lens`.
--
-- The isomorphism needs to be passed in twice to properly unify.

iso :: ArrowApply cat => Iso cat f o -> Iso cat g i -> Lens cat (f -> g) (o -> i)
iso (Iso f _) (Iso _ y) = lens f (app . arr (\(m, v) -> (y . m . f, v)))

-------------------------------------------------------------------------------

-- | Category instance for monomorphic lenses.

instance ArrowApply arr => Category (Lens arr) where
  id              = Id
  Lens f . Lens g = Lens (compose f g)
  Id     . u      = u
  u      . Id     = u
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Make a Lens output diverge by changing the input of the modifier. The
-- operator can be read as /points-to/.

infix 7 >-

(>-) :: Arrow arr => Lens arr (j -> a) (i -> b) -> Lens arr (f -> g) (o -> i) -> Point arr g j f o
(>-) (Lens (Point f _)) (Lens l) = Point (Point.get l) (Point.modify l . first (arr (f .)))
(>-) (Lens (Point f _)) Id       = Point id (app . first (arr (f .)))
(>-) Id                 l        = unpack l

-------------------------------------------------------------------------------

-- | Convert a polymorphic lens back to point.

unpack :: Lens cat (f -> g) (o -> i) -> Point cat g i f o
unpack Id       = identity
unpack (Lens p) = p

