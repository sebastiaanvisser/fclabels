{- | Lenses that allow polymorphic updates. -}

{-# LANGUAGE GADTs, TypeOperators #-}

module Data.Label.Poly where

import Control.Category
import Control.Arrow
import Data.Label.Abstract (Point (Point), _compose, _id, _get, _set, _modify)
import Prelude hiding ((.), id)

import qualified Data.Label.Abstract as A

-- | Polymorphic lens type.

data Lens cat p q where
  Lens :: Point cat g i f o -> Lens cat (f -> g) (o -> i)
  Id   :: Lens cat f f

-- | Polymorphic lenses form a `Category`.

instance ArrowApply arr => Category (Lens arr) where
  id              = Id
  Id     . u      = u
  u      . Id     = u
  Lens f . Lens g = Lens (_compose f g)

point :: ArrowApply arr => Lens arr (f -> g) (o -> i) -> Point arr g i f o
point Id       = _id
point (Lens p) = p

-- | Convert a monomorphic lens into a polymorphic lens.

poly :: A.Lens cat f a -> Lens cat (f -> f) (a -> a)
poly = Lens . A.unLens

-- | Convert a polymorphic lens into a monomorphic lens.

mono :: ArrowApply arr => Lens arr (f -> f) (a -> a) -> A.Lens arr f a
mono = A.Lens . point

-- | Operator for total polymorphic lenses.

type f :-> a = Lens (->) f a

-- | Operator for partial polymorphic lenses.

type f :~> a = Lens A.Partial f a

-------------------------------------------------------------------------------

-- | Getter for a polymorphic lens.

get :: ArrowApply arr => Lens arr (f -> g) (o -> i) -> arr f o
get = _get . point

-- | Setter for a polymorphic lens.

set :: ArrowApply arr => Lens arr (f -> g) (o -> i) -> arr (i, f) g
set = _set . point

-- | Modifier for a polymorphic lens.

modify :: ArrowApply arr => Lens arr (f -> g) (o -> i) -> arr (arr o i, f) g
modify = _modify . point












-------------------------------------------------------------------------------
-- Some example:

fstL :: ((o, b) -> (i, b)) :-> (o -> i)
fstL = Lens $ Point fst (\(f, (a, b)) -> (f a, b))

sndL :: ((a, o) -> (a, i)) :-> (o -> i)
sndL = Lens $ Point snd (\(f, (a, b)) -> (a, f b))

sndSnd :: ((a, (b, o)) -> (a, (b, i))) :-> (o -> i)
sndSnd = sndL . sndL

fstFst :: Lens (->) (((o, b), c) -> ((i, b), c)) (o -> i)
fstFst = fstL . fstL

fstSnd :: Lens (->) ((a, (o, c)) -> (a, (i, c))) (o -> i)
fstSnd = fstL . sndL

sndFst :: Lens (->) (((a, o), c) -> ((a, i), c)) (o -> i)
sndFst = sndL . fstL







data Triple a b c = Triple a b c

tripleA :: (Triple o b c -> Triple i b c) :-> (o -> i)
tripleA = Lens $ Point (\(Triple a _ _) -> a) (\(f, (Triple a b c)) -> (Triple (f a) b c))

tripleB :: (Triple a o c -> Triple a i c) :-> (o -> i)
tripleB = Lens $ Point (\(Triple _ b _) -> b) (\(f, (Triple a b c)) -> (Triple a (f b) c))

tripleC :: (Triple a b i -> Triple a b o) :-> (i -> o)
tripleC = Lens $ Point (\(Triple _ _ c) -> c) (\(f, (Triple a b c)) -> (Triple a b (f c)))

monofied2 :: A.Lens (->) (a, (b, Triple c d (e, f))) e
monofied2 = mono (fstL . tripleC . sndSnd)

