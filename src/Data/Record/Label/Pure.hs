{-# LANGUAGE
    TypeOperators
  , TupleSections
  #-}
module Data.Record.Label.Pure
( module Data.Record.Label.Abstract

, (:->)
, lens
, getL
, setL
, modL

, LensM
, getLM
, setLM
, modLM
)
where

import Control.Arrow
import Control.Category
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Record.Label.Abstract hiding (lens, getL, setL, modL)
import Prelude hiding ((.), id)
import qualified Data.Record.Label.Abstract as A

-- | Pure lens type specialized to pure functions.

type f :-> a = A.Lens (->) f a

-- | Create a pure lens from a getter and a setter.

lens :: (f -> a) -> (a -> f -> f) -> f :-> a
lens g s = A.lens g (uncurry s)

-- | Getter for a pure lens that cannot fail.

getL :: (f :-> a) -> f -> a
getL = A.getL

-- | Setter for a pure lens that cannot fail.

setL :: (f :-> a) -> a -> f -> f
setL = curry . A.setL

-- | Modifier for a pure lens that cannot fail.

modL :: (f :-> a) -> (a -> a) -> f -> f
modL = curry . A.modL

-- | Pure lens the type for situation in which getters and setter might fail.
-- This can be useful, for example, when accessing fields in datatypes with
-- multiple constructors.

type LensM f a = A.Lens (Kleisli (MaybeT Identity)) f a

runForLensM :: Kleisli (MaybeT Identity) f a -> f -> Maybe a
runForLensM l = runIdentity . runMaybeT . runKleisli l

-- | Getter for a lens that might fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

getLM :: LensM f a -> f -> Maybe a
getLM l = runForLensM (A.getL l)

-- | Setter for a lens that might fail. When the field to which the lens points
-- is not accessible this function behaves like the identity function.

setLM :: LensM f a -> a -> f -> f
setLM l v f = fromMaybe f (runForLensM (A.setL l . arr (v,)) f)

-- | Modifier for a lens that might fail. When the field to which the lens
-- points is not accessible this function behaves like the identity function.

modLM :: LensM f a -> (a -> a) -> f -> f
modLM l m f = fromMaybe f (runForLensM (A.modL l . arr (arr m,)) f)

