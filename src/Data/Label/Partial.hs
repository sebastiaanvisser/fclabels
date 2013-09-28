{-| Monomorphic lenses where the getters and updates can potentially silently
fail. Partial lenses are useful for creating accessor labels for multi
constructor data types where projection and modification of fields will not
always succeed.
-}

{-# LANGUAGE TypeOperators #-}
module Data.Label.Partial
( (:~>)
, Partial
, lens
, get
, modify
, set
, embed

-- * Seemingly total modifications.
, set'
, modify'
)
where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Label.Point (Partial)
import Data.Label.Poly (Lens)
import Data.Maybe
import Prelude hiding ((.), id)

import qualified Data.Label.Poly as Poly

{-# INLINE lens    #-}
{-# INLINE get     #-}
{-# INLINE modify  #-}
{-# INLINE set     #-}
{-# INLINE embed   #-}
{-# INLINE set'    #-}
{-# INLINE modify' #-}

-- | Partial lens type for situations in which the accessor functions can fail.

type f :~> o = Lens Partial f o

-------------------------------------------------------------------------------

-- | Create a lens that can fail from a getter and a modifier that can
-- themselves potentially fail.

lens :: (f -> Maybe o)                    -- ^ Getter.
     -> ((o -> Maybe i) -> f -> Maybe g)  -- ^ Modifier.
     -> (f -> g) :~> (o -> i)
lens g s = Poly.lens (Kleisli g) (Kleisli (\(m, f) -> s (runKleisli m) f))

-- | Getter for a lens that can fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

get :: (f -> g) :~> (o -> i) -> f -> Maybe o
get l = runKleisli (Poly.get l)

-- | Modifier for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

modify :: (f -> g) :~> (o -> i) -> (o -> i) -> f -> Maybe g
modify l m = runKleisli (Poly.modify l . arr ((,) (arr m)))

-- | Setter for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

set :: (f -> g) :~> (o -> i) -> i -> f -> Maybe g
set l v = runKleisli (Poly.set l . arr ((,) v))

-- | Embed a total lens that points to a `Maybe` field into a lens that might
-- fail.

embed :: Lens (->) (f -> g) (Maybe o -> Maybe i) -> (f -> g) :~> (o -> i)
embed l = lens (Poly.get l) (\m f -> const (Poly.modify l ((>>= m), f)) <$> Poly.get l f)

-------------------------------------------------------------------------------

-- | Like 'modify' but return behaves like the identity function when the field
-- could not be set.

modify' :: (f -> f) :~> (o -> o) -> (o -> o) -> f -> f
modify' l m f = f `fromMaybe` modify l m f

-- | Like 'set' but return behaves like the identity function when the field
-- could not be set.

set' :: (f -> f) :~> (o -> o) -> o -> f -> f
set' l v f = f `fromMaybe` set l v f

