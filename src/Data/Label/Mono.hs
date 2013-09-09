{- | Lenses that only allow monomorphic updates. Monomorphic lenses are simply
polymorphic lenses with the input and output type variables constraint to the
same type. -}

{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses #-}

module Data.Label.Mono
( Lens
, lens
, get
, set
, modify
)
where

import Control.Arrow
import Prelude hiding ((.), id, const, curry)

import qualified Data.Label.Poly as Poly

{-# INLINE lens   #-}
{-# INLINE get    #-}
{-# INLINE set    #-}
{-# INLINE modify #-}

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

-- | Get the setter arrow from a lens.

set :: Arrow arr => Lens arr f o -> arr (o, f) f
set = Poly.set

-- | Get the modifier arrow from a lens.

modify :: Lens cat f o -> cat (cat o o, f) f
modify = Poly.modify

