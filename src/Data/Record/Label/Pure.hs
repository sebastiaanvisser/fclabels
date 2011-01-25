{-# LANGUAGE TypeOperators #-}
module Data.Record.Label.Pure
( module Data.Record.Label.Abstract

, (:->)
, lens
, getL
, setL
, modL
)
where

import qualified Data.Record.Label.Abstract as A
import Data.Record.Label.Abstract hiding (lens, getL, setL, modL)

-- | Lens type specialized to functions.

type f :-> a = A.Lens (->) f a

lens :: (f -> a) -> (a -> f -> f) -> f :-> a
lens g s = A.lens g (uncurry s)

getL :: (f :-> a) -> f -> a
getL = A.getL

setL :: (f :-> a) -> a -> f -> f
setL = curry . A.setL

modL :: (f :-> a) -> (a -> a) -> f -> f
modL = curry . A.modL

