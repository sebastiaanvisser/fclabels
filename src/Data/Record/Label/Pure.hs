{-# LANGUAGE
    TypeOperators
  , TupleSections
  #-}
module Data.Record.Label.Pure
( (:->)
, lens
, getL
, setL
, modL

, (:~>)
, lensM
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

-- | Pure lens type for situation in which getters and setter might fail.
-- This can be useful, for example, when accessing fields in datatypes with
-- multiple constructors.

type f :~> a = A.Lens (Kleisli (MaybeT Identity)) f a

runForLensM :: Kleisli (MaybeT Identity) f a -> f -> Maybe a
runForLensM l = runIdentity . runMaybeT . runKleisli l

-- | Create a lens that might fail from a getter and a setter that can
-- themselves potentially fail.

lensM :: (f -> Maybe a) -> (a -> f -> Maybe f) -> (f :~> a)
lensM g s = A.lens (k g) (k (uncurry s))
  where k a = Kleisli (MaybeT . Identity . a)

-- | Getter for a lens that might fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

getLM :: (f :~> a) -> f -> Maybe a
getLM l = runForLensM (A.getL l)

-- | Setter for a lens that might fail. When the field to which the lens points
-- is not accessible this function returns `Nothing`.

setLM :: (f :~> a) -> a -> f -> Maybe f
setLM l v = runForLensM (A.setL l . arr (v,))

-- | Modifier for a lens that might fail. When the field to which the lens
-- points is not accessible this function returns `Nothing`.

modLM :: (f :~> a) -> (a -> a) -> f -> Maybe f
modLM l m = runForLensM (A.modL l . arr (arr m,))

