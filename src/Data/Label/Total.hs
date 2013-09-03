{-| Default lenses for simple total getters and updates. Useful for creating
accessor labels for single constructor datatypes. Also useful field labels that
are shared between all the constructors of a multi constructor datatypes.
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

import Data.Label.Abstract (Lens, Total)

import qualified Data.Label.Abstract as A

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
lens g s = A.lens g (uncurry s)

-- | Getter for a total lens.

get :: (f :-> a) -> f -> a
get = A.get

-- | Setter for a total lens.

set :: (f :-> a) -> a -> f -> f
set = curry . A.set

-- | Modifier for a total lens.

modify :: (f :-> a) -> (a -> a) -> f -> f
modify = curry . A.modify

