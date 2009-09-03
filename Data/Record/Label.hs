{-# LANGUAGE
    TypeOperators
  , TypeSynonymInstances
  , TemplateHaskell
 #-}
module Data.Record.Label
{-  (
  -- * Getter, setter and modifier types.
    Getter
  , Setter
  , Modifier

  -- * Label type.
  , Label (..)
  , get, set, mod
  , (:->) (..)

  -- * Bidirectional functor.
  , Lens (..)
  , (<->)
  , Iso (..)

  -- * State monadic label operations.

  , getM, setM, modM, (=:)

  -- * Derive labels using Template Haskell.
  , module Data.Record.Label.TH
  )-}
where

import Prelude hiding ((.), id, mod)
import Control.Applicative
import Control.Category
import Control.Monad.State hiding (get)
import Data.Record.Label.TH

type Getter   f o   = f -> o
type Setter   f i   = i -> f -> f
type Modifier f i o = (o -> i) -> f -> f

data Label f i o = Label
  { _get :: Getter f o
  , _set :: Setter f i
  }

_mod :: Label f i o -> (o -> i) -> f -> f
_mod l f a = _set l (f (_get l a)) a

newtype (f :-> a) = Wrap { unWrap :: Label f a a }

get :: (f :-> a) -> f -> a
get = _get . unWrap

set :: (f :-> a) -> a -> f -> f
set = _set . unWrap

mod :: (f :-> a) -> (a -> a) -> f -> f
mod = _mod . unWrap

instance Category (:->) where
  id = Wrap (Label id const)
  (Wrap a) . (Wrap b) = Wrap (Label (_get a . _get b) (_mod b . _set a))

instance Functor (Label f i) where
  fmap f x = Label (f . _get x) (_set x)

instance Applicative (Label f i) where
  pure a = Label (const a) (const id)
  a <*> b = Label (\f -> _get a f (_get b f)) (\r -> _set b r . _set a r)

class Iso f where
  iso :: Lens a b -> f a -> f b
  iso (Lens a b) = osi (b <-> a)
  osi :: Lens a b -> f b -> f a
  osi (Lens a b) = iso (b <-> a)

data Lens a b = Lens { fw :: a -> b, bw :: b -> a }

infixr 7 <->
(<->) :: (a -> b) -> (b -> a) -> Lens a b
a <-> b = Lens a b

instance Category Lens where
  id = Lens id id
  (Lens a b) . (Lens c d) = Lens (a . c) (d . b)

instance Iso ((:->) i) where
  iso l (Wrap a) = Wrap (Label (fw l . _get a) (_set a . bw l))

dimap :: (o' -> o) -> (i -> i') -> Label f i' o' -> Label f i o
dimap f g l = Label (f . _get l) (_set l . g)

data Person = Person
  { _name   :: String
  , _age    :: Int
  , _isMale :: Bool
  , _city   :: String
  } deriving Show

$(mkLabels [''Person])

city   :: Label Person String String
isMale :: Label Person Bool Bool
age    :: Label Person Int Int
name   :: Label Person String String

testUser :: Person
testUser = Person "sebas" 26 True "Utrecht"

ageAndCity :: Person :-> (Int, String)
ageAndCity = Wrap $ (,) <$> fst `for` age <*> snd `for` city

for :: (i -> i') -> Label f i' o' -> Label f i o'
for = dimap id

-- | Get a value out of state pointed to by the specified label.

getM :: MonadState s m => s :-> b -> m b
getM = gets . get

-- | Set a value somewhere in state pointed to by the specified label.

setM :: MonadState s m => s :-> b -> b -> m ()
setM l = modify . set l

-- | Alias for `setM' that reads like an assignment.

infixr 7 =:
(=:) :: MonadState s m => s :-> b -> b -> m ()
(=:) = setM

-- | Modify a value with a function somewhere in state pointed to by the
-- specified label.

modM :: MonadState s m => s :-> b -> (b -> b) -> m ()
modM l = modify . mod l

