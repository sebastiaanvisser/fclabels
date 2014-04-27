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
, modify
, set
, embed

-- * Seemingly total modifications.
, set'
, modify'

-- * Potentially removing modification.
, update
)
where

import Data.Label.Poly ((:->), (:~>))
import Data.Maybe

import qualified Data.Label.Total as Total
import qualified Data.Label.Poly  as Poly

{-# INLINE lens    #-}
{-# INLINE get     #-}
{-# INLINE modify  #-}
{-# INLINE set     #-}
{-# INLINE embed   #-}
{-# INLINE set'    #-}
{-# INLINE modify' #-}

-------------------------------------------------------------------------------

-- | Create a lens that can fail from a getter and a modifier that can
-- themselves potentially fail.

lens :: (f -> Maybe o)                    -- ^ Getter.
     -> ((o -> Maybe i) -> f -> Maybe g)  -- ^ Modifier.
     -> (f -> g) :~> (o -> i)
lens g s = Poly.lens g (\m f -> s m f)

-- | Getter for a lens that can fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

get :: (f -> g) :~> (o -> i)
    -> f
    -> Maybe o
get = Poly.get

-- | Modifier for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

modify :: (f -> g) :~> (o -> i)
       -> (o -> i)
       -> f
       -> Maybe g
modify l m = Poly.modify l (return . m)

-- | Like `modify`, but updates are allowed, depending on the underlying lens,
-- to remove items by modifying to `Nothing`.

update :: (f -> b) :~> (o -> i) -> (o -> Maybe i) -> f -> Maybe b
update = Poly.modify

-- | Setter for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

set :: (f -> g) :~> (o -> i)
    -> i
    -> f
    -> Maybe g
set l v = Poly.set l (return v)

-- | Embed a total lens that points to a `Maybe` field into a lens that might
-- fail.

embed :: (f -> g) :-> (Maybe o -> Maybe i)
      -> (f -> g) :~> (o -> i)
embed l = lens (Total.get l)
               (\m -> return . Total.modify l (maybe Nothing m))

-------------------------------------------------------------------------------

-- | Like 'modify' but return behaves like the identity function when the field
-- could not be set.

modify' :: (f -> f) :~> (o -> o) -> (o -> o) -> f -> f
modify' l m f = fromMaybe f (modify l m f)

-- | Like 'set' but return behaves like the identity function when the field
-- could not be set.

set' :: (f -> f) :~> (o -> o) -> o -> f -> f
set' l v f = fromMaybe f (set l v f)

