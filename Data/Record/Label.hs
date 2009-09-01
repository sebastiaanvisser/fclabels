{-# LANGUAGE TypeOperators #-}
module Data.Record.Label
  (
  -- * Getter, setter and modifier types.
    Getter
  , Setter
  , Modifier

  -- * Label type.
  , (:->) (..)
  , label

  -- * Bidirectional functor.

  , Lens
  , Iso (..)
  , (%)

  -- * State monadic label operations.

  , getM, setM, modM, (=:)

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

-- | Smart constructor for `Label's, the modifier will be computed based on
-- getter and setter.

label :: Getter a b -> Setter a b -> a :-> b
label g s = Label g s (\f a -> s (f (g a)) a)

instance Category (:->) where
  id = label id const
  (Label ga sa ma) . (Label gb _ mb) = Label (ga . gb) (mb . sa) (mb . ma)

type Lens a b = (a -> b, b -> a)

{- | Minimum definition is just one of the two. -}

class Iso f where
  iso :: Lens a b -> f a -> f b
  iso (a, b) = osi (b, a)
  osi :: Lens a b -> f b -> f a
  osi (a, b) = iso (b, a)

instance Iso ((:->) f) where
  iso (f, g) (Label a b c) = Label (f . a) (b . g) (c . (g.) . (.f))

-- | Apply label to lifted value and join afterwards.

infixr 8 %
(%) :: Functor f => a :-> b -> g :-> f a -> g :-> f b
(%) a b = let (Label g s _) = a in (fmap g, fmap (\k -> s k (error "unused"))) `iso` b

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
-- list :: Int -> [a] :-> a
-- list i = label (!! i) (\v a -> take i a ++ [v] ++ drop (i+1) a)

