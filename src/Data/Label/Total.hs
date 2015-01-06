{-| Default lenses for simple total getters and total possibly polymorphic,
updates. Useful for creating accessor labels for single constructor datatypes.
Also useful field labels that are shared between all the constructors of a
multi constructor datatypes.
-}

{-# LANGUAGE CPP, TypeOperators #-}

module Data.Label.Total
( (:->)
, Total
, lens
, get
, modify
, set

-- * Working in contexts.
, traverse
, lifted
)
where

#if MIN_VERSION_base(4,8,0)
import Prelude hiding (traverse)
#endif
import Control.Monad ((<=<), liftM)
import Data.Label.Poly (Lens)
import Data.Label.Point (Total)

import qualified Data.Label.Poly as Poly

{-# INLINE lens   #-}
{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}

-------------------------------------------------------------------------------

-- | Total lens type specialized for total accessor functions.

type f :-> o = Lens Total f o

-- | Create a total lens from a getter and a modifier.
--
-- We expect the following law to hold:
--
-- > get l (set l a f) == a
--
-- > set l (get l f) f == f

lens :: (f -> o)              -- ^ Getter.
     -> ((o -> i) -> f -> g)  -- ^ Modifier.
     -> (f -> g) :-> (o -> i)
lens g s = Poly.lens g (uncurry s)

-- | Get the getter function from a lens.

get :: ((f -> g) :-> (o -> i)) -> f -> o
get = Poly.get

-- | Get the modifier function from a lens.

modify :: (f -> g) :-> (o -> i) -> (o -> i) -> f -> g
modify = curry . Poly.modify

-- | Get the setter function from a lens.

set :: ((f -> g) :-> (o -> i)) -> i -> f -> g
set = curry . Poly.set

-- | Modify in some context.

traverse :: Functor m => (f -> g) :-> (o -> i) -> (o -> m i) -> f -> m g
traverse l m f = (\w -> set l w f) `fmap` m (get l f)


-- | Lifted lens composition.
--
-- For example, useful when specialized to lists:
--
-- > :: (f :-> [o])
-- > -> (o :-> [a])
-- > -> (f :-> [a])

lifted
  :: Monad m
  => (f -> g) :-> (m o -> m i)
  -> (o -> i) :-> (m a -> m b)
  -> (f -> g) :-> (m a -> m b)
lifted a b = lens (get b <=< get a) (modify a . liftM . modify b)

