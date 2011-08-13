{-# LANGUAGE TypeOperators #-}
module Data.Label.Pure
( (:->)
, lens
, get
, set
, modify
)
where

import qualified Data.Label.Abstract as A

type L f a = A.Lens (->) f a

-- | Pure lens type specialized for pure accessor functions.

type (f :-> a) = L f a

-- | Create a pure lens from a getter and a setter.
--
-- We expect the following law to hold:
--
-- > get l . set l a == const a

lens :: (f -> a) -> (a -> f -> f) -> f :-> a
lens g s = A.lens g (uncurry s)

-- | Getter for a pure lens.

get :: (f :-> a) -> f -> a
get = A.get

-- | Setter for a pure lens.

set :: (f :-> a) -> a -> f -> f
set = curry . A.set

-- | Modifier for a pure lens.

modify :: (f :-> a) -> (a -> a) -> f -> f
modify = curry . A.modify

