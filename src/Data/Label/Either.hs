{-# LANGUAGE TypeOperators, TupleSections #-}
module Data.Label.Either
( LensF
, Failing
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
import Prelude hiding ((.), id)
import Data.Label.Abstract (Lens, Failing)

import qualified Data.Label.Abstract as A

-- | Lens type for situations in which the accessor functions can fail with
-- some error information.

type LensF e f a = Lens (Failing e) f a

-- | Create a lens that can fail from a getter and a setter that can themselves
-- potentially fail.

lens :: (f -> Either e a) -> ((a -> Either e a) -> f -> Either e f) -> LensF e f a
lens g s = A.lens (Kleisli g) (Kleisli (\(m, f) -> s (runKleisli m) f))

-- | Getter for a lens that can fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

get :: LensF e f a -> f -> Either e a
get l = runKleisli (A.get l)

-- | Setter for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Left'.

set :: LensF e f a -> a -> f -> Either e f
set l v = runKleisli (A.set l . arr (v,))

-- | Like 'set' but return behaves like the identity function when the field
-- could not be set.

set' :: LensF e f a -> a -> f -> f
set' l v f = either (const f) id (set l v f)

-- | Modifier for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Left'.

modify :: LensF e f a -> (a -> a) -> f -> Either e f
modify l m = runKleisli (A.modify l . arr (arr m,))

-- | Like 'modify' but return behaves like the identity function when the field
-- could not be set.

modify' :: LensF a f a -> (a -> a) -> f -> f
modify' l m f = either (const f) id (modify l m f)

-- | Embed a total lens that points to an `Either` field into a lens that might
-- fail.

embed :: Lens (->) f (Either e a) -> LensF e f a
embed l = lens (A.get l) (\m f -> Right (A.modify l ((>>= m), f)))

