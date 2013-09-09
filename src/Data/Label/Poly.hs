{- | Lenses that allow polymorphic updates. -}

{-# LANGUAGE
    FlexibleInstances
  , GADTs
  , MultiParamTypeClasses
  , TypeOperators #-}

module Data.Label.Poly
(

-- * The polymorphic Lens type.
  Lens (Lens)
, point
, lens
, get
, set
, modify
, for
)
where

import Control.Category
import Control.Arrow
import Prelude hiding ((.), id)
import Data.Label.Point (Point (Point)) -- , Iso, Bijection (Bij))

import qualified Data.Label.Point as Point

{-# INLINE lens   #-}
{-# INLINE get    #-}
{-# INLINE set    #-}
{-# INLINE modify #-}
{-# INLINE for    #-}

-------------------------------------------------------------------------------

-- | Abstract polymorphic lens datatype. The getter and setter functions work
-- in some category. Categories allow for effectful lenses, for example, lenses
-- that might fail or use state.

data Lens cat f o where
  Lens :: Point cat g i f o -> Lens cat (f -> g) (o -> i)
  Id   :: ArrowApply cat => Lens cat f f

-- | Create a lens out of a getter and setter.

lens :: cat f o             -- ^ Getter.
     -> cat (cat o i, f) g  -- ^ Modifier.
     -> Lens cat (f -> g) (o -> i)
lens g m = Lens (Point g m)

-- | Get the getter arrow from a lens.

get :: Lens cat (f -> g) (o -> i) -> cat f o
get = Point.get . point

-- | Get the setter arrow from a lens.

set :: Arrow arr => Lens arr (f -> g) (o -> i) -> arr (i, f) g
set = Point.set . point

-- | Get the modifier arrow from a lens.

modify :: Lens cat (f -> g) (o -> i) -> cat (cat o i, f) g
modify = Point.modify . point

-------------------------------------------------------------------------------

-- | Category instance for monomorphic lenses.

instance ArrowApply arr => Category (Lens arr) where
  id              = Id
  Lens f . Lens g = Lens (Point.compose f g)
  Id     . u      = u
  u      . Id     = u
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Make a Lens output diverge by changing the input of the modifier.

for :: Arrow arr => arr j i -> Lens arr (f -> g) (o -> i) -> Point arr g j f o
for f (Lens l) = Point (Point.get l) (Point.modify l . first (arr (f .)))
for f Id       = Point id (app . first (arr (f .)))

-- We can diverge 'Lens'es using an isomorphism.
-- instance Arrow arr => Iso arr (Lens arr f) where
--   iso (Bij f b) (Lens (Point g m)) =
--     lens (f . g) (m . first (arr (\a -> b . a . f)))
--   {-# INLINE iso #-}

-------------------------------------------------------------------------------

-- | Convert a polymorphic lens back to point.

point :: Lens cat (f -> g) (o -> i) -> Point cat g i f o
point Id       = Point.id
point (Lens p) = p

