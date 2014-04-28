{- | The Label data type which generalizes the different lenses. -}

{-# LANGUAGE GADTs, FlexibleInstances #-}
module Data.Label.Label
(
-- * The point data type that generalizes lens.
  Label
, get
, modify
, set

, (>-)

-- * Working with isomorphisms.
, Iso (..)
, inv

)
where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category
import Control.Monad hiding (mapM)

{-# INLINE get    #-}
{-# INLINE modify #-}
{-# INLINE set    #-}
{-# INLINE inv    #-}

-------------------------------------------------------------------------------
-- | Abstract Label datatype. The getter and modifier operations work in some
-- monad. The type of the value pointed to might change, thereby changing the
-- type of the outer structure.

data Label m n f o where
  Ops :: !(f -> m o)
      -> !((o -> n i) -> f -> n g)
      -> Label m n (f -> g) (o -> i)
  Id :: Label m n f f

instance Monad m => Category (Label m n) where
  id                = Id
  Ops a b . Ops c d = Ops (a <=< c) (d . b)
  Id      . u       = u
  u       . Id      = u
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

get :: Monad m => Label m n (f -> g) (o -> i) -> f -> m o
get (Ops g _) = g
get Id        = return

modify :: Monad n => Label m n (f -> g) (o -> i) -> (o -> n i) -> f -> n g
modify (Ops _ m) = m
modify Id        = id

set :: Monad n => Label m n (f -> g) (o -> i) -> n i -> f -> n g
set l = modify l . const

-------------------------------------------------------------------------------
-- Applicative composition.

infix 7 >-

(>-) :: Monad m
     => Label m m (j -> a) (i -> b)
     -> Label m m (f -> g) (o -> i)
     -> Open m f g j o
(>-) (Ops f _) (Ops g m) = Open $ Ops g (\n -> m (f <=< n))
(>-) (Ops f _) Id        = Open $ Ops return (f <=<)
(>-) Id        l         = Open l

newtype Open m f g i o = Open (Label m m (f -> g) (o -> i))

instance Monad m => Functor (Open m f f i) where
  fmap f x = pure f <*> x
  {-# INLINE fmap #-}

instance Monad m => Applicative (Open m f f i) where
  pure a = Open $ Ops (const (return a)) (const return)
  Open a <*> Open b = Open $ Ops
    (liftM2 ap (get a) (get b))
    (\m f -> modify b (\y -> get a f >>= m . ($ y))
         =<< modify a (\x -> get b f >>= m . (x $)) f
    )
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance MonadPlus m => Alternative (Open m f f i) where
  empty             = Open $ Ops (const mzero) (const (const mzero))
  Open a <|> Open b = Open $ Ops (liftM2 mplus (get a) (get b))
                                 (\m -> liftM2 mplus (modify a m) (modify b m))

-------------------------------------------------------------------------------
-- | Affectful isomorphisms.

infix 8 `Iso`

-- | An isomorphism is like a `Category` that works in two directions.

data Iso m n i o = Iso { fw :: i -> m o, bw :: o -> n i }

-- | Isomorphisms are categories.

instance (Monad m, Monad n) => Category (Iso m n) where
  id = Iso return return
  Iso a b . Iso c d = Iso (a <=< c) (d <=< b)
  {-# INLINE id  #-}
  {-# INLINE (.) #-}

-- | Flip an isomorphism.

inv :: Iso m n i o -> Iso n m o i
inv i = Iso (bw i) (fw i)

