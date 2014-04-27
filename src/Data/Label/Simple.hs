{- | Lenses that only allow monomorphic total updates. -}

{-# LANGUAGE TypeOperators #-}

module Data.Label.Simple
( (:->)
, lens
, get
, modify
, set

, label
, iso
)
where

import Control.Monad.Identity (Identity, runIdentity)
import Data.Label.Label (Label, Iso (..))

import qualified Data.Label.Poly  as Poly
import qualified Data.Label.Total as Total

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

type f :-> o = (f -> f) Total.:-> (o -> o)

-- | Create a lens out of a getter and setter.

lens :: (f -> o)              -- ^ Getter.
     -> ((o -> o) -> f -> f)  -- ^ Modifier.
     -> f :-> o
lens = Total.lens

-- | Get the getter arrow from a lens.

get :: f :-> o -> f -> o
get = Total.get

-- | Get the modifier arrow from a lens.

modify :: f :-> o -> (o -> o) -> f -> f
modify = Total.modify

-- | Get the setter arrow from a lens.

set :: f :-> o -> o -> f -> f
set = Total.set

-- | Create lens from a `Label`.

label :: Label Identity Identity f o f o -> f :-> o
label = Poly.label

-- | Lift an isomorphism into a `Lens`.

iso :: Iso Identity Identity f o -> f :-> o
iso (Iso f b) = lens (runIdentity . f) (\m -> (runIdentity . b) . m . (runIdentity . f))

