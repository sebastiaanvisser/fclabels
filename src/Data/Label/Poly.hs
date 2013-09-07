{- | Lenses that allow polymorphic updates. -}

{-# LANGUAGE GADTs, TypeOperators #-}

module Data.Label.Poly where

import Control.Category
import Control.Arrow
import Data.Label.Abstract (Point (Point))
import Prelude hiding ((.), id, const)

import qualified Data.Label.Abstract as A

-- Pair-wise composition.

data Lens cat p q where
  Lens :: A.Point cat g i f o
       -> Lens cat (f -> g) (o -> i)
  Id   :: Lens cat  f        f

instance ArrowApply arr => Category (Lens arr) where
  id              = Id
  Id     . u      = u
  u      . Id     = u
  Lens f . Lens g = Lens (A._compose f g)

point :: ArrowApply arr => Lens arr (f -> g) (o -> i) -> A.Point arr g i f o
point Id       = A._id
point (Lens p) = p

poly :: A.Lens cat f a -> Lens cat (f -> f) (a -> a)
poly = Lens . A.unLens

mono :: ArrowApply arr => Lens arr (f -> f) (a -> a) -> A.Lens arr f a
mono = A.Lens . point

type f :-> a = Lens (->) f a

-------------------------------------------------------------------------------

get :: ArrowApply arr => Lens arr (f -> g) (o -> i) -> arr f o
get = A._get . point

set :: ArrowApply arr => Lens arr (f -> g) (o -> i) -> arr (i, f) g
set = A._set . point

modify :: ArrowApply arr => Lens arr (f -> g) (o -> i) -> arr (arr o i, f) g
modify = A._modify . point













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

