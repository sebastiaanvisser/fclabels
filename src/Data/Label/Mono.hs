{- | Lenses that only allow monomorphic updates. Monomorphic lenses are simply
polymorphic lenses with the input and output type variables constraint to the
same type. -}

{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , TypeOperators
  #-}

module Data.Label.Mono
( Lens
, lens
, get
, modify
, label
, set
, iso

-- * Specialized monomorphic lens operators.
, (:->)
, (:~>)
)
where

import Control.Monad
import Control.Monad.Identity (Identity)
import Data.Label.Label (Label, Iso (..))

import qualified Data.Label.Poly as Poly

{-# INLINE lens   #-}
{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}
{-# INLINE label  #-}
{-# INLINE iso    #-}

-------------------------------------------------------------------------------

-- | Abstract monomorphic lens datatype. The getter and setter functions work
-- in some category. Categories allow for effectful lenses, for example, lenses
-- that might fail or use state.

type Lens m f o = Poly.Lens m (f -> f) (o -> o)

-- | Create a lens out of a getter and setter.

lens :: (f -> m o)                -- ^ Getter.
     -> ((o -> m o) -> f -> m f)  -- ^ Modifier.
     -> Lens m f o
lens = Poly.lens

-- | Get the getter arrow from a lens.

get :: Lens m f o -> f -> m o
get = Poly.get

-- | Get the modifier arrow from a lens.

modify :: Lens m f o -> (o -> m o) -> f -> m f
modify = Poly.modify

-- | Get the setter arrow from a lens.

set :: Lens m f o -> m o -> f -> m f
set = Poly.set

-- | Create lens from a `Label`.

label :: Label m m f o f o -> Lens m f o
label = Poly.label

-- | Lift an isomorphism into a `Lens`.

iso :: Monad m => Iso m m f o -> Lens m f o
iso (Iso f b) = lens f (\m -> b <=< m <=< f)

-------------------------------------------------------------------------------

-- | Total monomorphic lens.

type f :-> o = Lens Identity f o

-- | Partial monomorphic lens.

type f :~> o = Lens Maybe f o

