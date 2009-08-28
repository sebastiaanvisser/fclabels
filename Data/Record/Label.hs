{-# LANGUAGE TypeOperators #-}
module Data.Record.Label
  (
  -- * Getter, setter and modifier types.
    Getter
  , Setter
  , Modifier

  -- * Label type.
  , (:->) (..)
  , mkModifier
  , mkLabel

  -- * Identity and composition.
  , idL
  , (%)

  -- * Bidirectional functor.

  , Lens (..)
  , (%%)

  -- * State monadic label operations.

  , getM, setM, modM, (=:)

  -- * Convenient label for list indexing.
  , list

  -- * Derive labels using Template Haskell.
  , module Data.Record.Label.TH

  ) where

import Prelude hiding ((.), id)
import Control.Category
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

-- | Create a modifier function out of a getter and a setter.

mkModifier :: Getter a b -> Setter a b -> Modifier a b
mkModifier gg ss f a = ss (f (gg a)) a

-- | Smart constructor for `Label's, the modifier will be computed based on
-- getter and setter.

mkLabel :: Getter a b -> Setter a b -> a :-> b
mkLabel g s = Label g s (mkModifier g s)

idL :: a :-> a
idL = mkLabel id const

infixr 8 %
(%) :: (g :-> a) -> (f :-> g) -> (f :-> a)
a % b = Label (lget a . lget b) (lmod b . lset a) (lmod b . lmod a)

instance Category (:->) where
  id = idL
  (.) = (%)

-- Apply custom `parser' and 'printer' function. This can be seen as a
-- bidirectional functorial map.

class Lens f where
  lmap :: (a -> b, b -> a) -> f a -> f b

instance Lens ((:->) f) where
  lmap (f, g) (Label a b c) = Label (f . a) (b . g) (c . (g.) . (.f))

-- | Apply label to lifted value and join afterwards.

infixr 8 %%
(%%) :: Functor f => a :-> b -> g :-> f a -> g :-> f b
(%%) a b = let (Label g s _) = a in (fmap g, fmap (flip s undefined)) `lmap` b

-- | Get a value out of state pointed to by the specified label.

getM :: MonadState s m => s :-> b -> m b
getM = gets . lget

-- | Set a value somewhere in state pointed to by the specified label.

setM :: MonadState s m => s :-> b -> b -> m ()
setM l = modify . lset l

-- | Alias for `setM' that reads like an assignment.

infixr 7 =:
(=:) :: MonadState s m => s :-> b -> b -> m ()
(=:) = setM

-- | Modify a value with a function somewhere in state pointed to by the
-- specified label.

modM :: MonadState s m => s :-> b -> (b -> b) -> m ()
modM l = modify . lmod l

-- Lift list indexing to a label.

list :: Int -> [a] :-> a
list i = mkLabel (!! i) (\v a -> take i a ++ [v] ++ drop (i+1) a)

