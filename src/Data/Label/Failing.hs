{-| Lenses for getters and updates that can potentially fail with some error
value. Like partial lenses, failing lenses are useful for creating accessor
labels for multi constructor data types where projection and modification of
fields will not always succeed. The error value can be used to report what
caused the failure.
-}

{-# LANGUAGE TypeOperators, TupleSections #-}

module Data.Label.Failing
( Lens
, Failing
, lens
, get
, set
, modify
, embed

-- * Seemingly total modifications.
, set'
, modify'
)
where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Data.Label.Point (Failing)

import qualified Data.Label.Mono as Mono

-- | Lens type for situations in which the accessor functions can fail with
-- some error information.

type Lens e f o = Mono.Lens (Failing e) f o

-- | Create a lens that can fail from a getter and a modifier that can
-- themselves potentially fail.

lens :: (f -> Either e o)                       -- ^ Getter.
     -> ((o -> Either e o) -> f -> Either e f)  -- ^ Modifier.
     -> Lens e f o
lens g s = Mono.lens (Kleisli g) (Kleisli (\(m, f) -> s (runKleisli m) f))

-- | Getter for a lens that can fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

get :: Lens e f o -> f -> Either e o
get l = runKleisli (Mono.get l)

-- | Setter for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Left'.

set :: Lens e f o -> o -> f -> Either e f
set l v = runKleisli (Mono.set l . arr (v,))

-- | Modifier for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Left'.

modify :: Lens e f o -> (o -> o) -> f -> Either e f
modify l m = runKleisli (Mono.modify l . arr (arr m,))

-- | Embed a total lens that points to an `Either` field into a lens that might
-- fail.

embed :: Mono.Lens (->) f (Either e o) -> Lens e f o
embed l = lens (Mono.get l) (\m f -> Right (Mono.modify l ((>>= m), f)))

-------------------------------------------------------------------------------

-- | Like 'set' but return behaves like the identity function when the field
-- could not be set.

set' :: Lens e f o -> o -> f -> f
set' l v f = either (const f) id (set l v f)

-- | Like 'modify' but return behaves like the identity function when the field
-- could not be set.

modify' :: Lens e f o -> (o -> o) -> f -> f
modify' l m f = either (const f) id (modify l m f)

