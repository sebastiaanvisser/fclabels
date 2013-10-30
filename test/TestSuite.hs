{-  OPTIONS -ddump-splices #-}

{-# LANGUAGE
    NoMonomorphismRestriction
  , GADTs
  , TemplateHaskell
  , TypeOperators
  , RankNTypes
  , FlexibleContexts #-}

module Main where

import Control.Arrow
import Control.Applicative
import Control.Category
import Prelude hiding ((.), id)
import Test.HUnit
import Data.Label
import Data.Label.Derive (defaultNaming, mkLabelsWith)
import Data.Label.Mono ((:~>))
import Data.Label.Failing (Failing)
import Data.Tuple (swap)

import Control.Monad.Reader (runReader)
import Control.Monad.State (evalState, execState, runState)

import qualified Data.Label.Base    as L
import qualified Data.Label.Failing as Failing
import qualified Data.Label.Mono    as Mono
import qualified Data.Label.Partial as Partial
import qualified Data.Label.Poly    as Poly
import qualified Data.Label.Total   as Total
import qualified Data.Label.Monadic as Monadic

-------------------------------------------------------------------------------

data NoRecord = NoRecord Integer Bool
  deriving (Eq, Ord, Show)

mkLabel ''NoRecord

fclabels [d|
  newtype Newtype a = Newtype { unNewtype :: [a] }
    deriving (Eq, Ord, Show)
  |]

newtypeL :: ArrowApply cat => Poly.Lens cat (Newtype a -> Newtype b) ([a] -> [b])
newtypeL = unNewtype

data Record = Record
  { _fA :: Integer
  , _fB :: Maybe (Newtype Bool)
  , _fC :: Newtype Bool
  , _fD :: Either Integer Bool
  } deriving (Eq, Ord, Show)

mkLabelsWith defaultNaming False False False False ''Record

fD :: ArrowApply cat => Mono.Lens cat (Record) (Either Integer Bool)
fC :: ArrowApply cat => Mono.Lens cat (Record) (Newtype Bool)
fB :: ArrowApply cat => Mono.Lens cat (Record) (Maybe (Newtype Bool))
fA :: ArrowApply cat => Mono.Lens cat (Record) (Integer)

data Multi
  = First  { _mA :: Record
           , _mB :: Double
           , _mC :: Either String Float
           }
  | Second { _mB :: Double }
  deriving (Eq, Ord, Show)

mkLabels [''Multi]

data View = View
  { _vA :: Maybe (Newtype Bool)
  , _vB :: Either Integer Bool
  , _vC :: Newtype Bool
  } deriving (Eq, Ord, Show)

mkLabelsWith defaultNaming True True False False ''View

data Direction i a b c d
  = North { _dir :: i, _north :: a }
  | East  { _dir :: i, _east  :: b }
  | South { _dir :: i, _south :: c }
  | West  { _dir :: i, _west  :: d }
  | All   { _dir :: i, _allDirs  :: (a, b, c, d) }
  deriving (Eq, Ord, Show)

mkLabelsWith defaultNaming True False True True ''Direction

-------------------------------------------------------------------------------

data Gadt a where
  C1 :: { ga :: Integer, gb :: Bool       } -> Gadt (Int, Bool)
  C2 :: { gc :: Integer, gd :: Maybe Bool } -> Gadt Bool
  C3 :: { ge :: a,       gf :: b          } -> Gadt (a, b)
  C4 :: { gg :: a                         } -> Gadt [a]
  C5 :: {                gd :: Maybe Bool } -> Gadt Bool
  C6 :: { gh :: [a]                       } -> Gadt (a, a, a)

mkLabel ''Gadt

_Ga :: (ArrowApply cat, ArrowChoice cat, ArrowZero cat) => Mono.Lens cat (Gadt (Int, Bool)) Integer
_Gb :: (ArrowApply cat, ArrowChoice cat, ArrowZero cat) => Mono.Lens cat (Gadt (Int, Bool)) Bool
_Gc :: (ArrowApply cat, ArrowChoice cat, ArrowZero cat) => Mono.Lens cat (Gadt Bool) Integer
_Gd :: (ArrowApply cat                                ) => Mono.Lens cat (Gadt Bool) (Maybe Bool)
_Ge :: (ArrowApply cat, ArrowChoice cat, ArrowZero cat) => Poly.Lens cat (Gadt (a, b) -> Gadt (c, b)) (a -> c)
_Gf :: (ArrowApply cat, ArrowChoice cat, ArrowZero cat) => Poly.Lens cat (Gadt (a, b) -> Gadt (a, c)) (b -> c)
_Gg :: (ArrowApply cat                                ) => Poly.Lens cat (Gadt [a] -> Gadt [b]) (a -> b)
_Gh :: (ArrowApply cat                                ) => Poly.Lens cat (Gadt (a, a, a) -> Gadt (b, b, b)) ([a] -> [b])

_Ga = lGa; _Gb = lGb; _Gc = lGc; _Gd = lGd; _Ge = lGe; _Gf = lGf; _Gg = lGg; _Gh = lGh;

-------------------------------------------------------------------------------

embed_fB :: Record :~> Newtype Bool
embed_fB = Partial.embed fB

manual_fA :: Record :-> Integer
manual_fA = Total.lens _fA (\m f -> f { _fA = m (_fA f) })

manual_fA_m :: Mono.Lens (->) Record Integer
manual_fA_m = lens _fA (\m f -> f { _fA = m (_fA f) })

manual_mA :: Multi :~> Record
manual_mA = Partial.lens
  (\p   -> case p of First {} -> Just (_mA p); _ -> Nothing)
  (\m p -> case p of First {} -> (\v -> p { _mA = v }) `fmap` m (_mA p); _ -> Nothing)

mA_f :: Failing.Lens String (Multi -> Multi) (Record -> Record)
mA_f = mA

manual_mA_f :: Failing.Lens String (Multi -> Multi) (Record -> Record)
manual_mA_f = Failing.lens
  (\p   -> case p of First {} -> Right (_mA p); _ -> Left "mA")
  (\m p -> case p of First {} -> (\v -> p { _mA = v }) `fmap` m (_mA p); _ -> Left "mA")

embed_fD :: Failing.Lens Integer (Record -> Record) (Bool -> Bool)
embed_fD = Failing.embed fD

manual_dir :: Poly.Lens (->) (Direction i a b c d -> Direction e a b c d) (i -> e)
manual_dir = Poly.lens _dir (\(m, f) -> f {_dir = m (_dir f) })

north_f :: Poly.Lens (Failing String) (Direction i a b c d -> Direction i e b c d) (a -> e)
north_f = north

fAmA :: Multi :~> Integer
fAmA = fA . mA

recordView :: Record :-> View
recordView = Poly.point $
  View <$> vA >- fB
       <*> vB >- fD
       <*> vC >- fC

newtypeId :: Newtype Bool :-> Newtype Bool
newtypeId = Poly.point (id <$> id >- id)

-------------------------------------------------------------------------------

fclabels [d|

  data View2 a
    = Con1 { field1 :: Bool
           , field2 :: (a, a)
           }
    | Con2 { field1 :: Bool
           , field3 :: [a]
           }
    deriving (Eq, Show)

  |]

view :: View2 a :~> Either (Bool, (a, a)) (Bool, [a])
view = point $
      Left  <$> L.left  >- con1
  <|> Right <$> L.right >- con2
  where con1 = point $
          (,) <$> L.fst >- field1
              <*> L.snd >- field2
        con2 = point $
          (,) <$> L.fst >- field1
              <*> L.snd >- field3

-------------------------------------------------------------------------------
-- Test data type with large number (> 26) of fields.

fclabels [d|

  data C = C { c_a :: (),  c_b :: (),  c_c  :: (),  c_d  :: (),  c_e  :: (),  c_f  :: ()
             , c_g :: (),  c_h :: (),  c_i  :: (),  c_j  :: (),  c_k  :: (),  c_l  :: ()
             , c_m :: (),  c_n :: (),  c_o  :: (),  c_p  :: (),  c_q  :: (),  c_r  :: ()
             , c_s :: (),  c_t :: (),  c_u  :: (),  c_v  :: (),  c_w  :: (),  c_x  :: ()
             , c_y :: (),  c_z :: (),  c_a0 :: (),  c_b0 :: (),  c_c0 :: (),  c_d0 :: ()
             }

  |]

-------------------------------------------------------------------------------

newtype0, newtype1, newtype2 :: Newtype Bool
newtype0 = Newtype []
newtype1 = Newtype [True]
newtype2 = Newtype [False]

record0, record1, record2, record3, record4, record5, record10, record11 :: Record
record0 = Record 0 Nothing newtype0 (Left 1)
record1 = Record 1 Nothing newtype0 (Left 1)
record2 = Record 0 (Just newtype1) newtype0 (Left 1)
record3 = Record 0 (Just newtype0) newtype0 (Left 1)
record4 = Record 0 Nothing newtype0 (Right True)
record5 = Record 0 Nothing newtype0 (Right False)
record10 = Record 10 Nothing newtype0 (Left 1)
record11 = Record 11 Nothing newtype0 (Left 1)

first0, first1, first2 :: Multi
first0 = First record0 0.0 (Right 1.0)
first1 = First record0 1.0 (Right 1.0)
first2 = First record1 0.0 (Right 1.0)

second0, second1 :: Multi
second0 = Second 0.0
second1 = Second 1.0

north0 :: Direction Integer () () () ()
north0 = North 0 ()

north1 :: Direction Bool () () () ()
north1 = North False ()

north2 :: Direction Integer Bool () () ()
north2 = North 0 False

west0 :: Direction Integer () () () ()
west0 = West 0 ()

mulDiv :: Iso (->) Integer Double
mulDiv = Iso (\i -> fromInteger i / 10) (\i -> round (i * 10))

addSub :: Iso (->) Double Integer
addSub = Iso (\i -> round (i + 10)) (\i -> fromInteger i - 10)

-------------------------------------------------------------------------------

main :: IO ()
main =
  do _ <- runTestTT allTests
     return ()

allTests :: Test
allTests = TestList
  [ mono
  , totalMono
  , partialMono
  , failingMono
  , totalPoly
  , partialPoly
  , failingPoly
  , composition
  , applicativeTotal
  , applicativePartial
  , bijections
  , monadic
  , base
  ]

mono :: Test
mono = TestList
  [ eq "get manual_fA_m" (get manual_fA_m record0) 0
  , eq "set manual_fA_m" (set manual_fA_m 1 record0) record1
  , eq "mod manual_fA_m" (modify manual_fA_m (+ 1) record0) record1
  ] where eq x = equality ("total mono " ++ x)

totalMono :: Test
totalMono = TestList
  [ eq "get fA" (Total.get fA record0) 0
  , eq "set fA" (Total.set fA 1 record0) record1
  , eq "mod fA" (Total.modify fA (+ 1) record0) record1
  , eq "get manual_fA" (Total.get manual_fA record0) 0 
  , eq "set manual_fA" (Total.set manual_fA 1 record0) record1
  , eq "mod manual_fA" (Total.modify manual_fA (+ 1) record0) record1
  , eq "get mB" (Total.get mB first0) 0
  , eq "set mB" (Total.set mB 1 first0) first1
  , eq "mod mB" (Total.modify mB (+ 1) first0) first1
  ] where eq x = equality ("total mono " ++ x)

partialMono :: Test
partialMono = TestList
  [ eq0 "get mA" (Partial.get mA first0) (Just record0)
  , eq0 "set mA" (Partial.set mA record1 first0) (Just first2)
  , eq0 "mod mA" (Partial.modify mA (Total.modify fA (+ 1)) first0) (Just first2)
  , eq0 "get manual_mA" (Partial.get manual_mA first0) (Just record0)
  , eq0 "set manual_mA" (Partial.set manual_mA record1 first0) (Just first2)
  , eq0 "mod manual_mA" (Partial.modify manual_mA (Total.modify fA (+ 1)) first0) (Just first2)
  , eq1 "get mA" (Partial.get mA second0) Nothing
  , eq1 "set mA" (Partial.set mA record1 second0) Nothing
  , eq1 "mod mA" (Partial.modify mA (Total.modify fA (+ 1)) second0) Nothing
  , eq1 "get manual_mA" (Partial.get manual_mA second0) Nothing
  , eq1 "set manual_mA" (Partial.set manual_mA record1 second0) Nothing
  , eq1 "mod manual_mA" (Partial.modify manual_mA (Total.modify fA (+ 1)) second0) Nothing
  , eq2 "set mA" (Partial.set' mA record1 first0) first2
  , eq2 "mod mA" (Partial.modify' mA (Total.modify fA (+ 1)) first0) first2
  , eq2 "set manual_mA" (Partial.set' manual_mA record1 first0) first2
  , eq2 "mod manual_mA" (Partial.modify' manual_mA (Total.modify fA (+ 1)) first0) first2
  , eq2 "set mA" (Partial.set' mA record1 second0) second0
  , eq2 "mod mA" (Partial.modify' mA (Total.modify fA (+ 1)) second0) second0
  , eq2 "set manual_mA" (Partial.set' manual_mA record1 second0) second0
  , eq2 "mod manual_mA" (Partial.modify' manual_mA (Total.modify fA (+ 1)) second0) second0
  , eq3 "get embed_fB" (Partial.get embed_fB record2) (Just newtype1)
  , eq3 "set embed_fB" (Partial.set embed_fB newtype0 record2) (Just record3)
  , eq3 "mod embed_fB" (Partial.modify embed_fB (const newtype0) record2) (Just record3)
  , eq4 "get embed_fB" (Partial.get embed_fB record0) Nothing
  , eq4 "set embed_fB" (Partial.set embed_fB newtype0 record0) Nothing
  , eq4 "mod embed_fB" (Partial.modify embed_fB (const newtype0) record0) Nothing
  ] where eq0 x = equality ("partial mono " ++ x)
          eq1 x = equality ("partial mono fail " ++ x)
          eq2 x = equality ("partial mono prime " ++ x)
          eq3 x = equality ("partial mono embed " ++ x)
          eq4 x = equality ("partial mono embed fail" ++ x)

failingMono :: Test
failingMono = TestList
  [ eq0 "get mA_f" (Failing.get mA_f first0) (Right record0)
  , eq0 "set mA_f" (Failing.set mA_f record1 first0) (Right first2)
  , eq0 "mod mA_f" (Failing.modify mA_f (Total.modify fA (+ 1)) first0) (Right first2)
  , eq0 "get manual_mA_f" (Failing.get manual_mA_f first0) (Right record0)
  , eq0 "set manual_mA_f" (Failing.set manual_mA_f record1 first0) (Right first2)
  , eq0 "mod manual_mA_f" (Failing.modify manual_mA_f (Total.modify fA (+ 1)) first0) (Right first2)
  , eq1 "get mA_f fail" (Failing.get mA_f second0) (Left "")
  , eq1 "set mA_f fail" (Failing.set mA_f record1 second0) (Left "")
  , eq1 "mod mA_f fail" (Failing.modify mA_f (Total.modify fA (+ 1)) second0) (Left "")
  , eq1 "get manual_mA_f" (Failing.get manual_mA_f second0) (Left "mA")
  , eq1 "set manual_mA_f" (Failing.set manual_mA_f record1 second0) (Left "mA")
  , eq1 "mod manual_mA_f" (Failing.modify manual_mA_f (Total.modify fA (+ 1)) second0) (Left "mA")
  , eq2 "set mA_f" (Failing.set' mA_f record1 first0) first2
  , eq2 "mod mA_f" (Failing.modify' mA_f (Total.modify fA (+ 1)) first0) first2
  , eq2 "set manual_mA_f" (Failing.set' manual_mA_f record1 first0) first2
  , eq2 "mod manual_mA_f" (Failing.modify' manual_mA_f (Total.modify fA (+ 1)) first0) first2
  , eq2 "set mA_f" (Failing.set' mA_f record1 second0) second0
  , eq2 "mod mA_f" (Failing.modify' mA_f (Total.modify fA (+ 1)) second0) second0
  , eq2 "set manual_mA_f" (Failing.set' manual_mA_f record1 second0) second0
  , eq2 "mod manual_mA_f" (Failing.modify' manual_mA_f (Total.modify fA (+ 1)) second0) second0
  , eq3 "get embed_fD" (Failing.get embed_fD record4) (Right True)
  , eq3 "set embed_fD" (Failing.set embed_fD False record4) (Right record5)
  , eq3 "mod embed_fD" (Failing.modify embed_fD not record4) (Right record5)
  , eq4 "get embed_fD" (Failing.get embed_fD record0) (Left 1)
  , eq4 "set embed_fD" (Failing.set embed_fD False record0) (Left 1)
  , eq4 "mod embed_fD" (Failing.modify embed_fD not record0) (Left 1)
  ] where eq0 x = equality ("failing mono " ++ x)
          eq1 x = equality ("failing mono fail " ++ x)
          eq2 x = equality ("failing mono prime " ++ x)
          eq3 x = equality ("failing mono embed " ++ x)
          eq4 x = equality ("failing mono embed fail " ++ x)

totalPoly :: Test
totalPoly = TestList
  [ eq "get dir" (Total.get dir north0) (0 :: Integer)
  , eq "set dir" (Total.set dir False north0) north1
  , eq "mod dir" (Total.modify dir (> 1) north0) north1
  , eq "get manual_dir" (Total.get manual_dir north0) 0
  , eq "set manual_dir" (Total.set manual_dir False north0) north1
  , eq "mod manual_dir" (Total.modify manual_dir (> 1) north0) north1
  ] where eq x = equality ("total mono " ++ x)

partialPoly :: Test
partialPoly = TestList
  [ eq0 "get north" (Partial.get north north0) (Just ())
  , eq0 "set north" (Partial.set north False north0) (Just north2)
  , eq0 "mod north" (Partial.modify north (> ()) north0) (Just north2)
  , eq1 "get north" (Partial.get north west0) Nothing
  , eq1 "set north" (Partial.set north False west0) Nothing
  , eq1 "mod north" (Partial.modify north (> ()) west0) Nothing
  ] where eq0 x = equality ("partial poly " ++ x)
          eq1 x = equality ("partial poly fail " ++ x)

failingPoly :: Test
failingPoly = TestList
  [ eq0 "get north" (Failing.get north_f north0) (Right ())
  , eq0 "set north" (Failing.set north_f False north0) (Right north2)
  , eq0 "mod north" (Failing.modify north_f (> ()) north0) (Right north2)
  , eq1 "get north" (Failing.get north_f west0) (Left "north")
  , eq1 "set north" (Failing.set north_f False west0) (Left "north")
  , eq1 "mod north" (Failing.modify north_f (> ()) west0) (Left "north")
  ] where eq0 x = equality ("failing poly " ++ x)
          eq1 x = equality ("failing poly fail " ++ x)

composition :: Test
composition = TestList
  [ eq0 "get id" (Partial.get id first0) (Just first0)
  , eq0 "set id" (Partial.set id first2 first0) (Just first2)
  , eq0 "mod id" (Partial.modify id (const first2) first0) (Just first2)
  , eq0 "get fAmA" (Partial.get fAmA first0) (Just 0)
  , eq0 "set fAmA" (Partial.set fAmA 1 first0) (Just first2)
  , eq0 "mod fAmA" (Partial.modify fAmA (+ 1) first0) (Just first2)
  , eq0 "get id fAmA" (Partial.get (id . fAmA) first0) (Just 0)
  , eq0 "set id fAmA" (Partial.set (id . fAmA) 1 first0) (Just first2)
  , eq0 "mod id fAmA" (Partial.modify (id . fAmA) (+ 1) first0) (Just first2)
  , eq0 "get fAmA id" (Partial.get (fAmA . id) first0) (Just 0)
  , eq0 "set fAmA id" (Partial.set (fAmA . id) 1 first0) (Just first2)
  , eq0 "mod fAmA id" (Partial.modify (fAmA . id) (+ 1) first0) (Just first2)
  ] where eq0 x = equality ("composition partial mono" ++ x)

applicativeTotal :: Test
applicativeTotal = TestList
  [ eq "get vA" (Total.get (vA . recordView) record0) Nothing
  , eq "get vB" (Total.get (vB . recordView) record0) (Left 1)
  , eq "get vC" (Total.get (vC . recordView) record0) newtype0
  , eq "set vA" (Total.set (vA . recordView) (Just newtype0) record2) record3
  , eq "modify vA" (Total.modify (vA . recordView) (fmap (const newtype0)) record2) record3

  , eq "get newtypeId" (Total.get newtypeId newtype0) newtype0
  , eq "set newtypeId" (Total.set newtypeId newtype1 newtype0) newtype1
  , eq "mod newtypeId" (Total.modify newtypeId (const newtype2) newtype0) newtype2
  ] where eq x = equality ("applicative total mono" ++ x)

myCon1 :: View2 Char
myCon1 = Con1 False ('a', 'z')

myCon2 :: View2 Char
myCon2 = Con2 True "abc"

applicativePartial :: Test
applicativePartial = TestList
  [ eq "get" (Partial.get    (L.snd . L.left  . view) myCon1) (Just ('a', 'z'))
  , eq "get" (Partial.get    (L.snd . L.left  . view) myCon2) Nothing
  , eq "get" (Partial.get    (L.snd . L.right . view) myCon1) Nothing
  , eq "get" (Partial.get    (L.snd . L.right . view) myCon2) (Just "abc")
  , eq "mod" (Partial.modify (L.fst . L.left  . view) not myCon1) (Just (Con1 True ('a', 'z')))
  , eq "mod" (Partial.modify (L.fst . L.left  . view) not myCon2) Nothing
  , eq "mod" (Partial.modify (L.fst . L.right . view) not myCon1) Nothing
  , eq "mod" (Partial.modify (L.fst . L.right . view) not myCon2) (Just (Con2 False "abc"))
  , eq "mod" (Partial.modify (L.snd . L.left  . view) swap myCon1) (Just (Con1 False ('z', 'a')))
  , eq "mod" (Partial.modify (L.snd . L.left  . view) swap myCon2) Nothing
  , eq "mod" (Partial.modify (L.snd . L.right . view) reverse myCon1) Nothing
  , eq "mod" (Partial.modify (L.snd . L.right . view) reverse myCon2) (Just (Con2 True "cba"))
  ] where eq x = equality ("applicative partial mono" ++ x)

bijections :: Test
bijections = TestList
  [ eq "get mulDiv" (get (iso mulDiv . fA) record0) 0
  , eq "set mulDiv" (set (iso mulDiv . fA) 1 record0) record10
  , eq "mod mulDiv" (modify (iso mulDiv . fA) (+ 1) record0) record10
  , eq "get addSub" (get (iso (inv addSub) . fA) record0) (-10)
  , eq "set addSub" (set (iso (inv addSub) . fA) 1 record0) record11
  , eq "mod addSub" (modify (iso (inv addSub) . fA) (+ 1) record0) record1

  , eq "get id mulDiv" (get (iso (id . mulDiv) . fA) record0) 0
  , eq "set id mulDiv" (set (iso (id . mulDiv) . fA) 1 record0) record10
  , eq "mod id mulDiv" (modify (iso (id . mulDiv) . fA) (+ 1) record0) record10
  , eq "get id mulDiv" (get (iso (mulDiv . id) . fA) record0) 0
  , eq "set id mulDiv" (set (iso (mulDiv . id) . fA) 1 record0) record10
  , eq "mod id mulDiv" (modify (iso (mulDiv . id) . fA) (+ 1) record0) record10
  ] where eq x = equality ("isomorphisms mono " ++ x)

monadic :: Test
monadic = TestList
  [ eq "asks id total" (runReader (Monadic.asks id) record0) record0
  , eq "asks fC total" (runReader (Monadic.asks fC) record0) newtype0
  , eq "gets id total" (evalState (Monadic.gets id) record0) record0
  , eq "gets fC total" (evalState (Monadic.gets fC) record0) newtype0

  , eq "put fA total" (execState (fA Monadic.=: 1) record0) record1
  , eq "modify fA total" (execState (fA Monadic.=. (+ 1)) record0) record1

  , eq "local fA total" (runReader (Monadic.local fA (+1) $ Monadic.asks id) record0) record1
  , eq "modifyAndGet fA total" (runState (Monadic.modifyAndGet fA (\a -> (a+10, a+1))) record0) (10, record1)
  ] where eq x = equality ("total monadic " ++ x)

base :: Test
base = TestList
  [ eq "get head" (Partial.get L.head [1, 2, 3]) (Just (1::Int))
  , eq "get head" (Partial.get L.head ([] :: [Int])) Nothing
  , eq "get tail" (Partial.get L.tail [1, 2, 3]) (Just [2, 3 ::Int])
  , eq "get tail" (Partial.get L.tail ([] :: [Int])) Nothing
  , eq "get left" (Partial.get L.left (Left 'a')) (Just 'a')
  , eq "get left" (Partial.get L.left (Right 'a' :: Either () Char)) Nothing
  , eq "get right" (Partial.get L.right (Right 'a')) (Just 'a')
  , eq "get right" (Partial.get L.right (Left 'a' :: Either Char ())) Nothing
  , eq "get just" (Partial.get L.just (Just 'a')) (Just 'a')
  , eq "get just" (Partial.get L.just (Nothing :: Maybe Char)) Nothing
  , eq "get fst" (Total.get (L.fst . L.swap) ('a', ())) ()
  , eq "get snd" (Total.get (L.snd . L.swap) ((), 'b')) ()
  , eq "get fst3" (Total.get L.fst3 ('a', (), ())) 'a'
  , eq "get snd3" (Total.get L.snd3 ((), 'b', ())) 'b'
  , eq "get trd3" (Total.get L.trd3 ((), (), 'c')) 'c'
  , eq "mod head" (Partial.modify L.head (*2) [1, 2, 3]) (Just [2, 2, 3::Int])
  , eq "mod head" (Partial.modify L.head (*2) ([]::[Int])) Nothing
  , eq "mod tail" (Partial.modify L.tail reverse [1, 2, 3]) (Just [1, 3, 2::Int])
  , eq "mod tail" (Partial.modify L.tail reverse ([]::[Int])) Nothing
  , eq "mod left" (Partial.modify L.left (=='a') (Left 'a')) (Just (Left True :: Either Bool ()))
  , eq "mod left" (Partial.modify L.left (=='a') (Right ())) (Nothing :: Maybe (Either Bool ()))
  , eq "mod right" (Partial.modify L.right (=='c') (Right 'b')) (Just (Right False :: Either () Bool))
  , eq "mod right" (Partial.modify L.right (=='c') (Left ())) (Nothing :: Maybe (Either () Bool))
  , eq "mod just" (Partial.modify L.just (=='a') (Just 'a')) (Just (Just True))
  , eq "mod just" (Partial.modify L.just (=='a') Nothing) Nothing
  , eq "mod fst" (Total.modify (L.fst . L.swap) (== 'a') ((), 'a')) ((), True)
  , eq "mod snd" (Total.modify (L.snd . L.swap) (== 'a') ('a', ())) (True, ())
  , eq "mod fst3" (Total.modify L.fst3 (== 'a') ('a', (), ())) (True, (), ())
  , eq "mod snd3" (Total.modify L.snd3 (== 'a') ((), 'b', ())) ((), False, ())
  , eq "mod trd3" (Total.modify L.trd3 (== 'a') ((), (), 'c')) ((), (), False)
  ] where eq x = equality ("base" ++ x)

xxx :: (b, o) :-> o
xxx = L.fst . L.swap

equality :: (Eq a, Show a) => String -> a -> a -> Test
equality d a b = TestCase (assertEqual d a b)

