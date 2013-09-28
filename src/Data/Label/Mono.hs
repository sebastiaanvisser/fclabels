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
, set
, iso

-- * Specialized monomorphic lens operators.
, (:->)
, (:~>)
)
where

import Control.Category
import Control.Arrow
import Data.Label.Point (Iso (..), Total, Partial)
import Prelude hiding ((.), id)

import qualified Data.Label.Poly as Poly

{-# INLINE lens   #-}
{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}
{-# INLINE iso    #-}

-------------------------------------------------------------------------------

-- | Abstract monomorphic lens datatype. The getter and setter functions work
-- in some category. Categories allow for effectful lenses, for example, lenses
-- that might fail or use state.

type Lens cat f o = Poly.Lens cat (f -> f) (o -> o)

-- | Create a lens out of a getter and setter.

lens :: cat f o               -- ^ Getter.
     -> (cat (cat o o, f) f)  -- ^ Modifier.
     -> Lens cat f o
lens = Poly.lens

-- | Get the getter arrow from a lens.

get :: Lens cat f o -> cat f o
get = Poly.get

-- | Get the modifier arrow from a lens.

modify :: Lens cat f o -> cat (cat o o, f) f
modify = Poly.modify

-- | Get the setter arrow from a lens.

set :: Arrow arr => Lens arr f o -> arr (o, f) f
set = Poly.set

-- | Lift an isomorphism into a `Lens`.

iso :: ArrowApply cat => Iso cat f o -> Lens cat f o
iso (Iso f b) = lens f (app . arr (\(m, v) -> (b . m . f, v)))

-------------------------------------------------------------------------------

-- | Total monomorphic lens.

type f :-> o = Lens Total f o

-- | Partial monomorphic lens.

type f :~> o = Lens Partial f o

