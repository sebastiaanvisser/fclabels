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
, set
, modify
, (>-)
)
where

import Control.Category
import Control.Arrow
import Prelude hiding ((.), id)
import Data.Label.Point (Point (Point))

import qualified Data.Label.Point as Point

{-# INLINE lens   #-}
{-# INLINE get    #-}
{-# INLINE set    #-}
{-# INLINE modify #-}
{-# INLINE (>-)   #-}
{-# INLINE point  #-}
{-# INLINE unpack #-}

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

-- | Create lens from a `Point`.

point :: Point cat g i f o -> Lens cat (f -> g) (o -> i)
point = Lens

-- | Get the getter arrow from a lens.

get :: Lens cat (f -> g) (o -> i) -> cat f o
get = Point.get . unpack

-- | Get the setter arrow from a lens.

set :: Arrow arr => Lens arr (f -> g) (o -> i) -> arr (i, f) g
set = Point.set . unpack

-- | Get the modifier arrow from a lens.

modify :: Lens cat (f -> g) (o -> i) -> cat (cat o i, f) g
modify = Point.modify . unpack

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

(>-) :: Arrow arr => arr j i -> Lens arr (f -> g) (o -> i) -> Point arr g j f o
(>-) f (Lens l) = Point (Point.get l) (Point.modify l . first (arr (f .)))
(>-) f Id       = Point id (app . first (arr (f .)))

-------------------------------------------------------------------------------

-- | Convert a polymorphic lens back to point.

unpack :: Lens cat (f -> g) (o -> i) -> Point cat g i f o
unpack Id       = Point.id
unpack (Lens p) = p

