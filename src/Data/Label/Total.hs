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

import Data.Label.Mono (Lens)
import Data.Label.Point (Total)

import qualified Data.Label.Mono as Mono

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
     -> ((o -> o) -> f -> f)  -- ^ Modifier.
     -> f :-> o
lens g s = Mono.lens g (uncurry s)

-- | Getter for a total lens.

get :: (f :-> o) -> f -> o
get = Mono.get

-- | Setter for a total lens.

set :: (f :-> o) -> o -> f -> f
set = curry . Mono.set

-- | Modifier for a total lens.

modify :: (f :-> o) -> (o -> o) -> f -> f
modify = curry . Mono.modify

