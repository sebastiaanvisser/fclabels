{-| Monomorphic lenses where the getters and updates can potentially silently
fail. Partial lenses are useful for creating accessor labels for multi
constructor data types where projection and modification of fields will not
always succeed.
-}

{-# LANGUAGE TypeOperators #-}
module Data.Label.Partial
( (:~>)
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
import Data.Label.Mono (Lens)
import Data.Label.Point (Partial)
import Data.Maybe
import Prelude hiding ((.), id)

import qualified Data.Label.Mono as Mono

-- | Partial lens type for situations in which the accessor functions can fail.

type f :~> o = Lens Partial f o

-- | Create a lens that can fail from a getter and a modifier that can
-- themselves potentially fail.

lens :: (f -> Maybe o)                    -- ^ Getter.
     -> ((o -> Maybe o) -> f -> Maybe f)  -- ^ Modifier.
     -> f :~> o
lens g s = Mono.lens (Kleisli g) (Kleisli (\(m, f) -> s (runKleisli m) f))

-- | Getter for a lens that can fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

get :: (f :~> o) -> f -> Maybe o
get l = runKleisli (Mono.get l)

-- | Setter for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

set :: f :~> o -> o -> f -> Maybe f
set l v = runKleisli (Mono.set l . arr ((,) v))

-- | Modifier for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

modify :: (f :~> o) -> (o -> o) -> f -> Maybe f
modify l m = runKleisli (Mono.modify l . arr ((,) (arr m)))

-- | Embed a total lens that points to a `Maybe` field into a lens that might
-- fail.

embed :: Lens (->) f (Maybe o) -> f :~> o
embed l = lens (Mono.get l) (\m f -> Just (Mono.modify l ((>>= m), f)))

-------------------------------------------------------------------------------

-- | Like 'set' but return behaves like the identity function when the field
-- could not be set.

set' :: (f :~> o) -> o -> f -> f
set' l v f = f `fromMaybe` set l v f

-- | Like 'modify' but return behaves like the identity function when the field
-- could not be set.

modify' :: (f :~> o) -> (o -> o) -> f -> f
modify' l m f = f `fromMaybe` modify l m f

