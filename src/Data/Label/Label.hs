{- | The Label data type which generalizes the different lenses.  -}

{-# LANGUAGE FlexibleInstances #-}
module Data.Label.Label
(
-- * The point data type that generalizes lens.
  Label (Label)
, get
, modify
, set
, identity
, compose

-- * Working with isomorphisms.
, Iso (..)
, inv
)
where

import Control.Applicative
import Control.Category
import Control.Monad
import Prelude hiding ((.), id)

{-# INLINE get      #-}
{-# INLINE modify   #-}
{-# INLINE set      #-}
{-# INLINE identity #-}
{-# INLINE compose  #-}
{-# INLINE inv      #-}

-------------------------------------------------------------------------------

-- | Abstract Label datatype. The getter and modifier operations work in some
-- monad. The type of the value pointed to might change, thereby changing the
-- type of the outer structure.

data Label m n g i f o =
  Label (f -> m o) ((o -> n i) -> f -> n g)

-- | Get the getter from a Label.

get :: Label m n g i f o -> f -> m o
get (Label g _) = g

-- | Get the modifier from a Label.

modify :: Label m n g i f o -> (o -> n i) -> f -> n g
modify (Label _ m) = m

-- | Get the setter from a Label.

set :: Label m n g i f o -> n i -> f -> n g
set l = modify l . const

-- | Identity Label.

identity :: (Monad n, Monad m) => Label m n f f o o
identity = Label return id

-- | Label composition.

compose :: Monad m
        => Label m n t i b o
        -> Label m n g t f b
        -> Label m n g i f o
compose (Label f m) (Label g n)
  = Label (f <=< g) (n . m)

-------------------------------------------------------------------------------

instance Monad m => Functor (Label m m f i f) where
  fmap f x = pure f <*> x
  {-# INLINE fmap #-}

instance Monad m => Applicative (Label m m f i f) where
  pure a  = Label (const (return a)) (const return)
  a <*> b = Label (liftM2 ap (get a) (get b))
                  (\m f -> modify b (\y -> get a f >>= m . ($ y))
                       =<< modify a (\x -> get b f >>= m . (x $)) f
                  )
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance MonadPlus m => Alternative (Label m m f i f) where
  empty   = Label (const mzero) (const (const mzero))
  a <|> b = Label (liftM2 mplus (get a) (get b))
                  (\m -> liftM2 mplus (modify a m) (modify b m))

-------------------------------------------------------------------------------

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

