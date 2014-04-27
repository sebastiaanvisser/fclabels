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

-- * Specialized polymorphic lens operators.
, (:->)
, (:~>)
)
where

import Prelude hiding ((.), id)

import Control.Category
import Control.Monad
import Control.Monad.Identity (Identity)
import Data.Label.Label (Label (Label), Iso(..), identity, compose)

import qualified Data.Label.Label as Label

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

data Lens m f o where
  Lens :: !(Label m m g i f o) -> Lens m (f -> g) (o -> i)
  Id   :: Monad m => Lens m f f

-- | Create a lens out of a getter and setter.

lens :: (f -> m o)                -- ^ Getter.
     -> ((o -> m i) -> f -> m g)  -- ^ Modifier.
     -> Lens m (f -> g) (o -> i)
lens g m = Lens (Label g m)

-- | Create lens from a `Label`.

point :: Label m m g i f o -> Lens m (f -> g) (o -> i)
point = Lens

-- | Get the getter arrow from a lens.

get :: Lens m (f -> g) (o -> i) -> f -> m o
get = Label.get . unpack

-- | Get the modifier arrow from a lens.

modify :: Lens m (f -> g) (o -> i) -> (o -> m i) -> f -> m g
modify = Label.modify . unpack

-- | Get the setter arrow from a lens.

set :: Lens m (f -> g) (o -> i) -> m i -> f -> m g
set = Label.set . unpack

-- | Lift a polymorphic isomorphism into a `Lens`.
--
-- The isomorphism needs to be passed in twice to properly unify.

iso :: Monad m => Iso m m f o -> Iso m m g i -> Lens m (f -> g) (o -> i)
iso (Iso f _) (Iso _ y) = lens f (\m -> y <=< m <=< f)

-------------------------------------------------------------------------------

-- | Category instance for polymorphic lenses.

instance Monad m => Category (Lens m) where
  id              = Id
  Lens f . Lens g = Lens (compose f g)
  Id     . u      = u
  u      . Id     = u
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Make a Lens output diverge by changing the input of the modifier. The
-- operator can be read as /points-to/.

infix 7 >-

(>-) :: Monad m => Lens m (j -> a) (i -> b) -> Lens m (f -> g) (o -> i) -> Label m m g j f o
(>-) (Lens (Label f _)) (Lens l) = Label (Label.get l) (\m -> Label.modify l (f <=< m))
(>-) (Lens (Label f _)) Id       = Label return (f <=<)
(>-) Id                 l        = unpack l

-------------------------------------------------------------------------------

-- | Convert a polymorphic lens back to point.

unpack :: Lens m (f -> g) (o -> i) -> Label m m g i f o
unpack Id       = identity
unpack (Lens p) = p

-------------------------------------------------------------------------------

-- | Total polymorphic lens.

type f :-> o = Lens Identity f o

-- | Partial polymorphic lens.

type f :~> o = Lens Maybe f o

