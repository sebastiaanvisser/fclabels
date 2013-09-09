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
import Data.Label.Point (Failing)
import Prelude hiding ((.), id)

import qualified Data.Label.Poly as Poly

{-# INLINE lens    #-}
{-# INLINE get     #-}
{-# INLINE set     #-}
{-# INLINE modify  #-}
{-# INLINE embed   #-}
{-# INLINE set'    #-}
{-# INLINE modify' #-}

-------------------------------------------------------------------------------
-- | Lens type for situations in which the accessor functions can fail with
-- some error information.

type Lens e f o = Poly.Lens (Failing e) f o

-- | Create a lens that can fail from a getter and a modifier that can
-- themselves potentially fail.

lens :: (f -> Either e o)                       -- ^ Getter.
     -> ((o -> Either e i) -> f -> Either e g)  -- ^ Modifier.
     -> Lens e (f -> g) (o -> i)
lens g s = Poly.lens (Kleisli g) (Kleisli (\(m, f) -> s (runKleisli m) f))

-- | Getter for a lens that can fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

get :: Lens e (f -> g) (o -> i) -> f -> Either e o
get l = runKleisli (Poly.get l)

-- | Setter for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Left'.

set :: Lens e (f -> g) (o -> i) -> i -> f -> Either e g
set l v = runKleisli (Poly.set l . arr (v,))

-- | Modifier for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Left'.

modify :: Lens e (f -> g) (o -> i) -> (o -> i) -> f -> Either e g
modify l m = runKleisli (Poly.modify l . arr (arr m,))

-- | Embed a total lens that points to an `Either` field into a lens that might
-- fail.

embed :: Poly.Lens (->) (f -> g) (Either e o -> Either e i) -> Lens e (f -> g) (o -> i)
embed l = lens (Poly.get l) (\m f -> Right (Poly.modify l ((>>= m), f)))

-------------------------------------------------------------------------------

-- | Like 'set' but return behaves like the identity function when the field
-- could not be set.

set' :: Lens e (f -> f) (o -> o) -> o -> f -> f
set' l v f = either (const f) id (set l v f)

-- | Like 'modify' but return behaves like the identity function when the field
-- could not be set.

modify' :: Lens e (f -> f) (o -> o) -> (o -> o) -> f -> f
modify' l m f = either (const f) id (modify l m f)

