{-| Default lenses for simple total getters and monomorphic updates. Useful for
creating accessor labels for single constructor datatypes. Also useful field
labels that are shared between all the constructors of a multi constructor
datatypes.
-}

{-# LANGUAGE TypeOperators #-}

module Data.Label.Total
( (:->)
, lens
, get
, set
, modify
)
where

import Data.Label.Poly (Lens)
import Data.Label.Point (Total)

import qualified Data.Label.Poly as Poly

{-# INLINE lens   #-}
{-# INLINE get    #-}
{-# INLINE set    #-}
{-# INLINE modify #-}

-------------------------------------------------------------------------------

-- | Total lens type specialized for total accessor functions.

type f :-> o = Lens Total f o

-- | Create a total lens from a getter and a modifier.
--
-- We expect the following law to hold:
--
-- > get l (set l a f) == a
--
-- > set l (get l f) f == f

lens :: (f -> o)              -- ^ Getter.
     -> ((o -> i) -> f -> g)  -- ^ Modifier.
     -> (f -> g) :-> (o -> i)
lens g s = Poly.lens g (uncurry s)

-- | Get the getter function from a lens.

get :: ((f -> g) :-> (o -> i)) -> f -> o
get = Poly.get

-- | Get the setter function from a lens.

set :: ((f -> g) :-> (o -> i)) -> i -> f -> g
set = curry . Poly.set

-- | Get the modifier function from a lens.

modify :: (f -> g) :-> (o -> i) -> (o -> i) -> f -> g
modify = curry . Poly.modify

