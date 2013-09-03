{-  OPTIONS -ddump-splices #-}
{-# LANGUAGE TemplateHaskell, TypeOperators, FlexibleContexts #-}

module Main where

import Control.Category
import Prelude hiding ((.), id)
import Test.HUnit
import Data.Label.Abstract
import Data.Label (mkLabels)
import Data.Label.Total   ((:->))
import Data.Label.Partial ((:~>))

import qualified Data.Label.Total    as Total
import qualified Data.Label.Partial  as Partial
import qualified Data.Label.Failing  as Failing
import qualified Data.Label.TotalM   as TotalM
import qualified Data.Label.PartialM as PartialM

-------------------------------------------------------------------------------

data NoRecord = NoRecord Int Bool
  deriving (Eq, Ord, Show)

newtype Newtype a = Newtype { _unNewtype :: [a] }
  deriving (Eq, Ord, Show)

data Tot = Tot
  { _fA :: Int
  , _fB :: Maybe (Newtype Bool)
  , _fC :: Newtype Bool
  , _fD :: Either Int Bool
  } deriving (Eq, Ord, Show)

data Part 
  = First  { _first  :: Tot
           , _second :: Double
           , _third  :: Either String Float
           }
  | Second { _second :: Double }
  deriving (Eq, Ord, Show)

mkLabels [''NoRecord, ''Newtype, ''Tot, ''Part]

-------------------------------------------------------------------------------

lfA :: Tot :-> Int
lfA = Total.lens _fA (\m f -> f { _fA = m (_fA f) })

lfst :: Part :~> Tot
lfst = Partial.lens
  (\p -> case p of
           First {} -> Just (_first p)
           _        -> Nothing
  )
  (\m p -> case p of
             First {} -> (\v -> p { _first = v }) `fmap` m (_first p)
             _        -> Nothing
  )

lfstF :: Lens (Failing String) Part Tot
lfstF = Failing.lens
  (\p -> case p of
           First {} -> Right (_first p)
           _        -> Left "lfst"
  )
  (\m p -> case p of
             First {} -> (\v -> p { _first = v }) `fmap` m (_first p)
             _        -> Left "lfst"
  )

fstF :: Lens (Failing String) Part Tot
fstF = first

compTot :: Tot :-> [Bool]
compTot = unNewtype . fC

compTotId :: Tot :-> [Bool]
compTotId = id . unNewtype . fC

compIdTot :: Tot :-> [Bool]
compIdTot = unNewtype . fC . id

compPart :: Part :~> Int
compPart = fA . first

compFail :: Lens (Failing String) Part Int
compFail = fA . first

fBembed :: Tot :~> Newtype Bool
fBembed = Partial.embed fB

fDembed :: Failing.LensF Int Tot Bool
fDembed = Failing.embed fD

-------------------------------------------------------------------------------

nt0, nt1 :: Newtype Bool
nt0 = Newtype []
nt1 = Newtype [True]

tot0, tot1, tot2, tot3, tot4, tot5, tot6, tot7 :: Tot
tot0 = Tot 0 Nothing nt0 (Left 1)
tot1 = Tot 1 (Just nt0) nt0 (Left 1)
tot2 = Tot 2 Nothing nt0 (Left 1)
tot3 = Tot 1 Nothing nt0 (Left 1)
tot4 = Tot 1 (Just nt1) nt0 (Left 1)
tot5 = Tot 0 Nothing nt1 (Left 1)
tot6 = Tot 0 Nothing nt1 (Right False)
tot7 = Tot 0 Nothing nt1 (Right True)

fst0, fst1, fst2, snd0 :: Part
fst0 = First tot0 0.0 (Right 10.0)
fst1 = First tot1 0.0 (Right 10.0)
fst2 = First tot0 2.0 (Right 10.0)
snd0 = Second 2.0

-------------------------------------------------------------------------------

main :: IO ()
main =
  do _ <- runTestTT allTests
     return ()

allTests :: Test
allTests = TestList
  [ total
  , partialOk
  , partialFail
  , partialEmbed
  , failingOk
  , failingFail
  , failingEmbed
  , compTotal
  ]

total :: Test
total = TestList
  [ eq "get fA total" (Total.get fA tot0) 0 
  , eq "set fA total" (Total.set fA 2 tot0) tot2 
  , eq "mod fA total" (Total.modify fA (+2) tot0) tot2
  , eq "get lfA total" (Total.get lfA tot0) 0
  , eq "set lfA total" (Total.set lfA 2 tot0) tot2
  , eq "mod lfA total" (Total.modify lfA (+2) tot0) tot2
  , eq "get fB total" (Total.get fB tot0) Nothing
  , eq "set fB total" (Total.set fB Nothing tot1) tot3
  , eq "mod fB total" (Total.modify fB (fmap (const nt1)) tot1) tot4
  , eq "get second total" (Total.get second fst0) 0
  , eq "set second total" (Total.set second 2 fst0) fst2
  , eq "mod second total" (Total.modify second (+2) fst0) fst2
  ]

partialOk :: Test
partialOk = TestList
  [ eq "get first partial ok" (Partial.get first fst0) (Just tot0)
  , eq "set first partial ok" (Partial.set first tot1 fst0) (Just fst1)
  , eq "mod first partial ok" (Partial.modify first (const tot1) fst0) (Just fst1)
  , eq "get lfst partial ok" (Partial.get lfst fst0) (Just tot0)
  , eq "set lfst partial ok" (Partial.set lfst tot1 fst0) (Just fst1)
  , eq "mod lfst partial ok" (Partial.modify lfst (const tot1) fst0) (Just fst1)
  , eq "set' first partial ok" (Partial.set' first tot1 fst0) fst1
  , eq "mod' first partial ok" (Partial.modify' first (const tot1) fst0) fst1
  ]

partialFail :: Test
partialFail = TestList
  [ eq "get first partial fail" (Partial.get first snd0) Nothing
  , eq "set first partial fail" (Partial.set first tot1 snd0) Nothing
  , eq "mod first partial fail" (Partial.modify first (const tot1) snd0) Nothing
  , eq "get lfst partial fail" (Partial.get lfst snd0) Nothing
  , eq "set lfst partial fail" (Partial.set lfst tot1 snd0) Nothing
  , eq "mod lfst partial fail" (Partial.modify lfst (const tot1) snd0) Nothing
  , eq "set' first partial fail" (Partial.set' first tot1 snd0) snd0
  , eq "mod' first partial fail" (Partial.modify' first (const tot1) snd0) snd0
  ]

partialEmbed :: Test
partialEmbed = TestList
  [ eq "get fB partial embed" (Partial.get fBembed tot1) (Just nt0)
  , eq "get fB partial embed" (Partial.get fBembed tot0) Nothing
  , eq "set fB partial embed" (Partial.set fBembed nt1 tot1) (Just tot4)
  , eq "mod fB partial embed" (Partial.modify fBembed (const nt1) tot1) (Just tot4)
  ]

failingOk :: Test
failingOk = TestList
  [ eq "get first failing ok" (Failing.get fstF fst0) (Right tot0)
  , eq "set first failing ok" (Failing.set fstF tot1 fst0) (Right fst1)
  , eq "mod first failing ok" (Failing.modify fstF (const tot1) fst0) (Right fst1)
  , eq "get lfstF failing ok" (Failing.get lfstF fst0) (Right tot0)
  , eq "set lfstF failing ok" (Failing.set lfstF tot1 fst0) (Right fst1)
  , eq "mod lfstF failing ok" (Failing.modify lfstF (const tot1) fst0) (Right fst1)
  , eq "set' first failing ok" (Failing.set' fstF tot1 fst0) fst1
  , eq "mod' first failing ok" (Failing.modify' fstF (const tot1) fst0) fst1
  ]

failingFail :: Test
failingFail = TestList
  [ eq "get first failing fail" (Failing.get first snd0) (Left "first")
  , eq "set first failing fail" (Failing.set first tot1 snd0) (Left "first")
  , eq "mod first failing fail" (Failing.modify first (const tot1) snd0) (Left "first")
  , eq "get lfstF failing fail" (Failing.get lfstF snd0) (Left "lfst")
  , eq "set lfstF failing fail" (Failing.set lfstF tot1 snd0) (Left "lfst")
  , eq "mod lfstF failing fail" (Failing.modify lfstF (const tot1) snd0) (Left "lfst")
  , eq "set' first failing fail" (Failing.set' fstF tot1 snd0) snd0
  , eq "mod' first failing fail" (Failing.modify' fstF (const tot1) snd0) snd0
  ]

failingEmbed :: Test
failingEmbed = TestList
  [ eq "get fD failing embed" (Failing.get fDembed tot1) (Left 1)
  , eq "get fD failing embed" (Failing.get fDembed tot6) (Right False)
  , eq "set fD failing embed" (Failing.set fDembed True tot1) (Right tot1)
  , eq "set fD failing embed" (Failing.set fDembed True tot6) (Right tot7)
  , eq "mod fD failing embed" (Failing.modify fDembed not tot1) (Right tot1)
  , eq "mod fD failing embed" (Failing.modify fDembed not tot6) (Right tot7)
  ]

compTotal :: Test
compTotal = TestList
  [ eq "get fB partial comp" (Total.get compTot tot0) []
  , eq "set fB partial comp" (Total.set compTot [True] tot0) tot5
  , eq "mod fB partial comp" (Total.modify compTot (True:) tot0) tot5
  , eq "get fB partial comp" (Total.get compTotId tot0) []
  , eq "set fB partial comp" (Total.set compTotId [True] tot0) tot5
  , eq "mod fB partial comp" (Total.modify compTotId (True:) tot0) tot5
  , eq "get fB partial comp" (Total.get compIdTot tot0) []
  , eq "set fB partial comp" (Total.set compIdTot [True] tot0) tot5
  , eq "mod fB partial comp" (Total.modify compIdTot (True:) tot0) tot5
  ]

eq :: (Eq a, Show a) => String -> a -> a -> Test
eq d a b = TestCase (assertEqual d a b)

