{-# LANGUAGE TypeOperators #-}
module Data.Record.Label
  ( Getter, Setter, Modifier
  , (:->) (..)
  , mkModifier
  , mkLabel
  , Lens (..)
  , (%)
  , getM, setM, modM, (=:)

  , enterM
  , enterMT
  , bothM
  , localM
  , withM

  , list
  , module Data.Record.Label.TH
  ) where

import Control.Monad.State
import Data.Record.Label.TH

type Getter   a b = a -> b
type Setter   a b = b -> a -> a
type Modifier a b = (b -> b) -> a -> a

data a :-> b = Label
  { lget :: Getter   a b
  , lset :: Setter   a b
  , lmod :: Modifier a b
  }

mkModifier :: Getter a b -> Setter a b -> Modifier a b
mkModifier gg ss f a = ss (f (gg a)) a

mkLabel :: Getter a b -> Setter a b -> a :-> b
mkLabel g s = Label g s (mkModifier g s)

infixr 8 %
(%) :: (g :-> a) -> (f :-> g) -> (f :-> a)
a % b = Label (lget a . lget b) (lmod b . lset a) (lmod b . lmod a)

-- Apply custom `parser' and 'printer' function. This can be seen as a
-- bidirectional functorial map.

class Lens f where
  lmap :: (a -> b, b -> a) -> f a -> f b

instance Lens ((:->) f) where
  lmap (f, g) (Label a b c) = Label (f . a) (b . g) (c . (g.) . (.f))

-- Extend the state monad with support for labels.

getM :: MonadState s m => s :-> b -> m b
getM = gets . lget

setM :: MonadState s m => s :-> b -> b -> m ()
setM l = modify . lset l

infixr 7 =:
(=:) :: MonadState s m => s :-> b -> b -> m ()
(=:) = setM

modM :: MonadState s m => s :-> b -> (b -> b) -> m ()
modM l = modify . lmod l

-- Run a state computation for a sub element updating this part of the state afterwards.

enterM :: MonadState s m => s :-> b -> State b b1 -> m b1
enterM l c = do
  b <- getM l
  let (a, s) = runState c b
  setM l s
  return a

enterMT
  :: (MonadState s (t m), MonadTrans t, Monad m)
  => s :-> b -> StateT b m a -> t m a
enterMT l c = do
  b <- getM l
  (a, s) <- lift $ runStateT c b
  setM l s
  return a

bothM :: MonadState s m => s :-> b -> State b b1 -> m (b, b1)
bothM parent cmp = do
  p <- getM parent
  c <- enterM parent cmp
  return (p, c)

localM :: MonadState s m => s :-> b -> m b1 -> m b1
localM l comp = do
  k <- getM l
  c <- comp
  setM l k
  return c

withM :: MonadState s m => s :-> b -> State b a -> m b1 -> m b1
withM l c d = localM l (enterM l c >> d)

-- Lift list indexing to a label.

list :: Int -> [a] :-> a
list i = mkLabel (!! i) (\v a -> take i a ++ [v] ++ drop (i+1) a)

