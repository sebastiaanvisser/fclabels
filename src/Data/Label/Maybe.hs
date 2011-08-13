{-# LANGUAGE TypeOperators, TupleSections #-}
module Data.Label.Maybe
( (:~>)
, lens
, get
, set
, modify
, embed
)
where

import Control.Arrow
import Control.Category
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.Maybe
import Prelude hiding ((.), id)
import qualified Data.Label.Abstract as A

type L f a = A.Lens (Kleisli (MaybeT Identity)) f a

-- | Lens type for situations in which the accessor functions can fail. This is
-- useful, for example, when accessing fields in datatypes with multiple
-- constructors.

type f :~> a = L f a

run :: Kleisli (MaybeT Identity) f a -> f -> Maybe a
run l = runIdentity . runMaybeT . runKleisli l

-- | Create a lens that can fail from a getter and a setter that can themselves
-- potentially fail.

lens :: (f -> Maybe a) -> (a -> f -> Maybe f) -> f :~> a
lens g s = A.lens (kl g) (kl (uncurry s))
  where kl a = Kleisli (MaybeT . Identity . a)

-- | Getter for a lens that can fail. When the field to which the lens points
-- is not accessible the getter returns 'Nothing'.

get :: (f :~> a) -> f -> Maybe a
get l = run (A.get l)

-- | Setter for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

set :: f :~> a -> a -> f -> Maybe f
set l v = run (A.set l . arr (v,))

-- | Modifier for a lens that can fail. When the field to which the lens points
-- is not accessible this function returns 'Nothing'.

modify :: (f :~> a) -> (a -> a) -> f -> Maybe f
modify l m = run (A.modify l . arr (arr m,))

-- | Embed a pure lens that points to a Maybe field into a lens that might
-- fail.

embed :: A.Lens (->) f (Maybe a) -> f :~> a
embed l = lens (A.get l) (\a f -> Just (A.set l (Just a, f)))

