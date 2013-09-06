{- | Lenses that allow polymorphic updates. -}

{-# LANGUAGE
    ConstraintKinds
  , GADTs
  , FlexibleInstances
  , FunctionalDependencies
  , KindSignatures
  , MultiParamTypeClasses
  , RankNTypes
  , TypeOperators
  , TypeFamilies
  , ViewPatterns
  #-}

module Data.Label.Poly where

import Control.Arrow
import Control.Category
import Data.Label.Abstract (Point (Point))
import Prelude hiding ((.), id, const)
import GHC.Exts (Constraint)

import qualified Data.Label.Abstract as A




-- Pair-wise composition.

infixr 9 !

class CategoryC cat where
  type C cat a b c :: Constraint
  ci  :: C cat a b c => cat a a
  (!) :: C cat a b c
      => cat b c
      -> cat a b
      -> cat a c

instance ArrowApply arr => CategoryC (A.Lens arr) where
  type C (A.Lens arr) a b c = ()
  ci = id
  (!) = (.)

-- instance ArrowApply cat => Bicategory (Point cat) where
--   (!) (Point gi mi) (Point go mo)
--     = Point (gi . go) (app . arr (first (\a -> mo . A.pack (mi . A.pack a))))

-- data a <> b = In a b

data Poly cat f o where
  Poly :: Point cat g i f o -> Poly cat (f -> g) (o -> i)

comp :: (a ~ (f -> g), b ~ (p -> j), c ~ (o -> i), ArrowApply cat)
     => Poly cat b c
     -> Poly cat a b
     -> Poly cat a c
comp (Poly (Point gi mi)) (Poly (Point go mo)) = Poly $
  Point (gi . go) (app . arr (first (\a -> mo . A.pack (mi . A.pack a))))

{-

-- Needs closed type families.

data Void2 a b
type family Bla a :: * -> * -> * where
  type instance Bla (a -> b) = (->)
  type instance Bla a        = Void2

instance ArrowApply cat => CategoryC (Poly cat) where
   type C (Poly cat) a b c = (Bla a ~ (->), Bla b ~ (->), Bla c ~ (->))
   (!) = comp

-}

-------------------------------------------------------------------------------

-- get :: Point cat g i f o -> cat f o
-- get = A._get

-- set :: Arrow arr => Point arr g i f o -> arr (i, f) g
-- set = A._set

-- modify :: Point cat g i f o -> cat (cat o i, f) g
-- modify = A._modify

-------------------------------------------------------------------------------

{-
ccc :: A.Lens (->) ((a, (b, (c, d))), e) d
ccc = A.Lens (sndL ! sndL ! sndL ! fstL)

aaa :: Point (->) (a, (b, c)) c (a, (b, o)) o
aaa = sndL ! sndL

bbb :: Point (->) ((i, b), c) i ((o, b), c) o
bbb = fstL ! fstL

xxx :: Point (->) (a, (i, b)) i (a, (o, b)) o
xxx = fstL ! sndL

yyy :: Point (->) ((a, i), b) i ((a, o), b) o
yyy = sndL ! fstL

-}

fstL :: Point (->) (a, b) a (d, b) d
fstL = Point fst (\(f, (a, b)) -> (f a, b))

sndL :: Point (->) (a, b) b (a, c) c
sndL = Point snd (\(f, (a, b)) -> (a, f b))

sndL_ :: Poly (->) ((a, o) -> (a, i)) (o -> i)
sndL_ = Poly sndL

data Triple a b c = Triple a b c

t0 :: Poly (->) (Triple o b c -> Triple i b c) (o -> i)
t0 = Poly $ Point (\(Triple a _ _) -> a) (\(f, (Triple a b c)) -> (Triple (f a) b c))

t1 :: Point (->) (Triple a b c) b (Triple a o c) o
t1 = Point (\(Triple _ b _) -> b) (\(f, (Triple a b c)) -> (Triple a (f b) c))

t2 :: Point (->) (Triple a b c) c (Triple a b o) o
t2 = Point (\(Triple _ _ c) -> c) (\(f, (Triple a b c)) -> (Triple a b (f c)))

