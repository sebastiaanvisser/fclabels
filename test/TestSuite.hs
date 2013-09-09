{- OPTIONS -ddump-splices #-}
{-# LANGUAGE
    TemplateHaskell
  , TypeOperators
  , FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Category
import Prelude hiding ((.), id)
import Test.HUnit
import Data.Label
import Data.Label.Mono (Lens)
import Data.Label.Partial ((:~>))
import Data.Label.Failing (Failing)

import Control.Monad.Reader (runReader)
import Control.Monad.State (evalState, execState, runState)

import qualified Data.Label.Failing  as Failing
import qualified Data.Label.Mono     as Mono ()
import qualified Data.Label.Partial  as Partial
import qualified Data.Label.Poly     as Poly
import qualified Data.Label.Total    as Total
import qualified Data.Label.Monadic  as Monadic

-------------------------------------------------------------------------------

data NoRecord = NoRecord Integer Bool
  deriving (Eq, Ord, Show)

newtype Newtype a = Newtype { _unNewtype :: [a] }
  deriving (Eq, Ord, Show)

data Record = Record
  { _fA :: Integer
  , _fB :: Maybe (Newtype Bool)
  , _fC :: Newtype Bool
  , _fD :: Either Integer Bool
  } deriving (Eq, Ord, Show)

data Multi
  = First  { _mA :: Record
           , _mB :: Double
           , _mC :: Either String Float
           }
  | Second { _mB :: Double }
  deriving (Eq, Ord, Show)

mkLabels [''NoRecord, ''Newtype, ''Record, ''Multi]

data View = View
  { _vA :: Maybe (Newtype Bool)
  , _vB :: Either Integer Bool
  , _vC :: Newtype Bool
  } deriving (Eq, Ord, Show)

mkLabel ''View

data Direction i a b c d
  = North { _dir :: i, _north :: a }
  | East  { _dir :: i, _east  :: b }
  | South { _dir :: i, _south :: c }
  | West  { _dir :: i, _west  :: d }
  | All   { _dir :: i, _allDirs  :: (a, b, c, d) }
  deriving (Eq, Ord, Show)

mkLabel ''Direction

-------------------------------------------------------------------------------

embed_fB :: (Record -> Record) :~> (Newtype Bool -> Newtype Bool)
embed_fB = Partial.embed fB

manual_fA :: Record :-> Integer
manual_fA = Total.lens _fA (\m f -> f { _fA = m (_fA f) })

manual_fA_m :: Lens (->) Record Integer
manual_fA_m = lens _fA (\m f -> f { _fA = m (_fA f) })

manual_mA :: (Multi -> Multi) :~> (Record -> Record)
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

fAmA :: (Multi -> Multi) :~> (Integer -> Integer)
fAmA = fA . mA

recordView :: Record :-> View
recordView = Poly.Lens $
  View <$> _vA `for` fB
       <*> _vB `for` fD
       <*> _vC `for` fC

newtypeId :: Newtype Bool :-> Newtype Bool
newtypeId = Poly.Lens (id <$> id `for` id)

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

mulDiv :: Isomorphism (->) Integer Double
mulDiv = Iso (\i -> fromInteger i / 10) (\i -> round (i * 10))

addSub :: Isomorphism (->) Double Integer
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
  , applicative
  , bijections
  , monadic
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
  , eq1 "get mA_f fail" (Failing.get mA_f second0) (Left "mA")
  , eq1 "set mA_f fail" (Failing.set mA_f record1 second0) (Left "mA")
  , eq1 "mod mA_f fail" (Failing.modify mA_f (Total.modify fA (+ 1)) second0) (Left "mA")
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

applicative :: Test
applicative = TestList
  [ eq "get vA" (Total.get (vA . recordView) record0) Nothing
  , eq "get vB" (Total.get (vB . recordView) record0) (Left 1)
  , eq "get vC" (Total.get (vC . recordView) record0) newtype0
  , eq "set vA" (Total.set (vA . recordView) (Just newtype0) record2) record3
  , eq "modify vA" (Total.modify (vA . recordView) (fmap (const newtype0)) record2) record3

  , eq "get newtypeId" (Total.get newtypeId newtype0) newtype0
  , eq "set newtypeId" (Total.set newtypeId newtype1 newtype0) newtype1
  , eq "mod newtypeId" (Total.modify newtypeId (const newtype2) newtype0) newtype2
  ] where eq x = equality ("applicative total mono" ++ x)

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

equality :: (Eq a, Show a) => String -> a -> a -> Test
equality d a b = TestCase (assertEqual d a b)
