{-# LANGUAGE TypeOperators, TupleSections #-}
module Data.Label.Maybe
( (:~>)
, Partial
, lens
, get
, set
, set'
, modify
, modify'
, embed
)
where

import Control.Arrow
import Control.Category
import Data.Maybe
import Prelude hiding ((.), id)
import Data.Label.Abstract (Lens, Partial)

import qualified Data.Label.Abstract as A

-- | Lens type for situations in which the accessor functions can fail. This is
-- useful, for example, when accessing fields in datatypes with multiple
-- constructors.

type f :~> a = Lens Partial f a

-- | Create a lens that can fail from a getter and a setter that can themselves
-- potentially fail.

lens :: (f -> Maybe a) -> ((a -> Maybe a) -> f -> Maybe f) -> f :~> a
lens g s = A.lens (Kleisli g) (Kleisli (\(m, f) -> s (runKleisli m) f))

-- | Getter for a lens that can fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

get :: (f :~> a) -> f -> Maybe a
get l = runKleisli (A.get l)

-- | Setter for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

set :: f :~> a -> a -> f -> Maybe f
set l v = runKleisli (A.set l . arr (v,))

-- | Like 'set' but return behaves like the identity function when the field
-- could not be set.

set' :: (f :~> a) -> a -> f -> f
set' l v f = f `fromMaybe` set l v f

-- | Modifier for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

modify :: (f :~> a) -> (a -> a) -> f -> Maybe f
modify l m = runKleisli (A.modify l . arr (arr m,))

-- | Like 'modify' but return behaves like the identity function when the field
-- could not be set.

modify' :: (f :~> a) -> (a -> a) -> f -> f
modify' l m f = f `fromMaybe` modify l m f

-- | Embed a total lens that points to a `Maybe` field into a lens that might
-- fail.

embed :: A.Lens (->) f (Maybe a) -> f :~> a
embed l = lens (A.get l) (\m f -> Just (A.modify l ((>>= m), f)))

