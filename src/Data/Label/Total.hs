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

type f :-> a = Lens Total f a

-- | Create a total lens from a getter and a modifier.
--
-- We expect the following law to hold:
--
-- > get l (set l a f) == a
--
-- > set l (get l f) f == f

lens :: (f -> a)              -- ^ Getter.
     -> ((a -> a) -> f -> f)  -- ^ Modifier.
     -> f :-> a
lens g s = Mono.lens g (uncurry s)

-- | Getter for a total lens.

get :: (f :-> a) -> f -> a
get = Mono.get

-- | Setter for a total lens.

set :: (f :-> a) -> a -> f -> f
set = curry . Mono.set

-- | Modifier for a total lens.

modify :: (f :-> a) -> (a -> a) -> f -> f
modify = curry . Mono.modify

