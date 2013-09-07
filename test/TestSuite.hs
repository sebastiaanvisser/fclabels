{-  OPTIONS -ddump-splices #-}
{-# LANGUAGE
    TemplateHaskell
  , TypeOperators
  , FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Category
import Prelude hiding ((.), id)
import Test.HUnit
import Data.Label.Abstract
import Data.Label (mkLabels)
import Data.Label.Total   ((:->))
import Data.Label.Partial ((:~>))

import Control.Monad.Reader (ReaderT, runReaderT, runReader)
import Control.Monad.State (evalStateT, evalState, execState, runState)

import qualified Data.Label.Total    as Total
import qualified Data.Label.Partial  as Partial
import qualified Data.Label.Failing  as Failing
import qualified Data.Label.TotalM   as TotalM
import qualified Data.Label.PartialM as PartialM
import qualified Data.Label.Poly     as Poly ()

-------------------------------------------------------------------------------

data NoRecord = NoRecord Integer Bool
  deriving (Eq, Ord, Show)

newtype Newtype a = Newtype { _unNewtype :: [a] }
  deriving (Eq, Ord, Show)

data Tot = Tot
  { _fA :: Integer
  , _fB :: Maybe (Newtype Bool)
  , _fC :: Newtype Bool
  , _fD :: Either Integer Bool
  } deriving (Eq, Ord, Show)

data App = App
  { _tA :: Maybe (Newtype Bool)
  , _tB :: Either Integer Bool
  , _tD :: Newtype Bool
  } deriving (Eq, Ord, Show)

data Part 
  = First  { _pfst  :: Tot
           , _psnd :: Double
           , _third  :: Either String Float
           }
  | Second { _psnd :: Double }
  deriving (Eq, Ord, Show)

mkLabels [''NoRecord, ''Newtype, ''Tot, ''App, ''Part]

-------------------------------------------------------------------------------

lfA :: Tot :-> Integer
lfA = Total.lens _fA (\m f -> f { _fA = m (_fA f) })

lfst :: Part :~> Tot
lfst = Partial.lens
  (\p   -> case p of First {} -> Just (_pfst p); _ -> Nothing)
  (\m p -> case p of First {} -> (\v -> p { _pfst = v }) `fmap` m (_pfst p); _ -> Nothing)

lfstF :: Lens (Failing String) Part Tot
lfstF = Failing.lens
  (\p   -> case p of First {} -> Right (_pfst p); _ -> Left "lfst")
  (\m p -> case p of First {} -> (\v -> p { _pfst = v }) `fmap` m (_pfst p); _ -> Left "lfst")

fstF :: Lens (Failing String) Part Tot
fstF = pfst

compTot :: Tot :-> [Bool]
compTot = unNewtype . fC

compTotId :: Tot :-> [Bool]
compTotId = id . unNewtype . fC

compIdTot :: Tot :-> [Bool]
compIdTot = unNewtype . fC . id

compPart :: Part :~> Integer
compPart = fA . pfst

compFail :: Lens (Failing String) Part Integer
compFail = fA . pfst

fBembed :: Tot :~> Newtype Bool
fBembed = Partial.embed fB

fDembed :: Failing.LensF Integer Tot Bool
fDembed = Failing.embed fD

totTot2 :: Tot :-> App
totTot2 = Lens $
  App <$> _tA `for` fB
      <*> _tB `for` fD
      <*> _tD `for` fC

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

fst0, fst1, fst2, snd0, snd1 :: Part
fst0 = First tot0 0.0 (Right 10.0)
fst1 = First tot1 0.0 (Right 10.0)
fst2 = First tot0 2.0 (Right 10.0)
snd0 = Second 2.0
snd1 = Second 4.0

app0, app1, app2, app3, app4, app5, app6, app7 :: App
app0 = App Nothing (Left 1) nt0
app1 = App (Just nt1) (Left 1) nt0
app2 = App Nothing (Left 1) nt0
app3 = App Nothing (Left 1) nt0
app4 = App (Just nt1) (Left 1) nt0
app5 = App Nothing (Left 1) nt1
app6 = App Nothing (Right False) nt1
app7 = App Nothing (Right True) nt1

mulDiv :: Bijection (->) Integer Double
mulDiv = Bij (\i -> fromInteger i / 10) (\i -> round (i * 10))

addSub :: Bijection (->) Integer Double
addSub = Bij (\i -> fromInteger i - 1) (\i -> round (i + 1))

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
  , totalM
  , partialM
  , bijections
  , applicative
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
  , eq "get psnd total" (Total.get psnd fst0) 0
  , eq "set psnd total" (Total.set psnd 2 fst0) fst2
  , eq "mod psnd total" (Total.modify psnd (+2) fst0) fst2
  ]

partialOk :: Test
partialOk = TestList
  [ eq "get pfst partial ok" (Partial.get pfst fst0) (Just tot0)
  , eq "set pfst partial ok" (Partial.set pfst tot1 fst0) (Just fst1)
  , eq "mod pfst partial ok" (Partial.modify pfst (const tot1) fst0) (Just fst1)
  , eq "get lfst partial ok" (Partial.get lfst fst0) (Just tot0)
  , eq "set lfst partial ok" (Partial.set lfst tot1 fst0) (Just fst1)
  , eq "mod lfst partial ok" (Partial.modify lfst (const tot1) fst0) (Just fst1)
  , eq "set' pfst partial ok" (Partial.set' pfst tot1 fst0) fst1
  , eq "mod' pfst partial ok" (Partial.modify' pfst (const tot1) fst0) fst1
  ]

partialFail :: Test
partialFail = TestList
  [ eq "get pfst partial fail" (Partial.get pfst snd0) Nothing
  , eq "set pfst partial fail" (Partial.set pfst tot1 snd0) Nothing
  , eq "mod pfst partial fail" (Partial.modify pfst (const tot1) snd0) Nothing
  , eq "get lfst partial fail" (Partial.get lfst snd0) Nothing
  , eq "set lfst partial fail" (Partial.set lfst tot1 snd0) Nothing
  , eq "mod lfst partial fail" (Partial.modify lfst (const tot1) snd0) Nothing
  , eq "set' pfst partial fail" (Partial.set' pfst tot1 snd0) snd0
  , eq "mod' pfst partial fail" (Partial.modify' pfst (const tot1) snd0) snd0
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
  [ eq "get pfst failing ok" (Failing.get fstF fst0) (Right tot0)
  , eq "set pfst failing ok" (Failing.set fstF tot1 fst0) (Right fst1)
  , eq "mod pfst failing ok" (Failing.modify fstF (const tot1) fst0) (Right fst1)
  , eq "get lfstF failing ok" (Failing.get lfstF fst0) (Right tot0)
  , eq "set lfstF failing ok" (Failing.set lfstF tot1 fst0) (Right fst1)
  , eq "mod lfstF failing ok" (Failing.modify lfstF (const tot1) fst0) (Right fst1)
  , eq "set' pfst failing ok" (Failing.set' fstF tot1 fst0) fst1
  , eq "mod' pfst failing ok" (Failing.modify' fstF (const tot1) fst0) fst1
  ]

failingFail :: Test
failingFail = TestList
  [ eq "get pfst failing fail" (Failing.get pfst snd0) (Left "pfst")
  , eq "set pfst failing fail" (Failing.set pfst tot1 snd0) (Left "pfst")
  , eq "mod pfst failing fail" (Failing.modify pfst (const tot1) snd0) (Left "pfst")
  , eq "get lfstF failing fail" (Failing.get lfstF snd0) (Left "lfst")
  , eq "set lfstF failing fail" (Failing.set lfstF tot1 snd0) (Left "lfst")
  , eq "mod lfstF failing fail" (Failing.modify lfstF (const tot1) snd0) (Left "lfst")
  , eq "set' pfst failing fail" (Failing.set' fstF tot1 snd0) snd0
  , eq "mod' pfst failing fail" (Failing.modify' fstF (const tot1) snd0) snd0
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

totalM :: Test
totalM = TestList
  [ eq "asks id total" (runReader (TotalM.asks id) tot0) (tot0)
  , eq "asks fC total" (runReader (TotalM.asks fC) tot0) nt0
  , eq "asks psnd total" (runReader (TotalM.asks psnd) fst0) 0.0
  , eq "gets id total" (evalState (TotalM.gets id) tot0) tot0
  , eq "gets fC total" (evalState (TotalM.gets fC) tot0) nt0
  , eq "gets psnd total" (evalState (TotalM.gets psnd) fst0) 0.0
  , eq "put psnd total" (execState (psnd TotalM.=: 2) fst0) fst2
  , eq "modify psnd total" (execState (psnd TotalM.=. (+2)) fst0) fst2
  , eq "local psnd total" (runReader (TotalM.local psnd (+2) $ TotalM.asks id) fst0) fst2
  , eq "modifyAndGet fA total" (runState (TotalM.modifyAndGet psnd (\a -> (a+10, a+2))) fst0) (10, fst2)
  ]

partialM :: Test
partialM = TestList
  [ eq "asks id partial" (runReaderT (PartialM.asks id) tot0) (Just tot0)
  , eq "asks fC partial" (runReaderT (PartialM.asks fC) tot0) (Just nt0)
  , eq "asks psnd partial" (runReaderT (PartialM.asks psnd) fst0) (Just 0.0)
  , eq "asks pfst partial" (runReaderT (PartialM.asks pfst) snd0) Nothing
  , eq "gets id partial" (evalStateT (PartialM.gets id) tot0) (Just tot0)
  , eq "gets fC partial" (evalStateT (PartialM.gets fC) tot0) (Just nt0)
  , eq "gets psnd partial" (evalStateT (PartialM.gets psnd) fst0) (Just 0.0)
  , eq "gets pfst partial" (evalStateT (PartialM.gets pfst) snd0) Nothing
  ]

bijections :: Test
bijections = TestList
  [ eq "fw bij" (fw mulDiv 10) 1.0
  , eq "bw bij" (bw mulDiv 1.0) 10
  , eq "fw id bij" (fw id 10) (10 :: Integer)
  , eq "bw id bij" (bw id 10) (10 :: Integer)
  , eq "fw id comp bij" (fw (id . mulDiv) 10) 1.0
  , eq "bw id comp bij" (bw (id . mulDiv) 1.0) 10
  , eq "fw comp bij" (fw (inv addSub . mulDiv) 10) 2
  , eq "bw comp bij" (bw (inv addSub . mulDiv) 2) 10
  , eq "fw comp iso bij" (fw (inv addSub `iso` mulDiv) 10) 2
  , eq "bw comp iso bij" (bw (inv addSub `iso` mulDiv) 2) 10
  , eq "fw lift bij" (fw (liftBij (addSub . inv mulDiv)) [10, 4]) [99, 39]
  , eq "bw lift bij" (bw (liftBij (addSub . inv mulDiv)) [2, 3]) [0.3, 0.4]
  , eq "get fA bij total" (Total.get (mulDiv `iso` fA) tot1) 0.1
  , eq "get psnd bij inv total" (Total.get (inv mulDiv `iso` psnd) snd0) 20
  , eq "modify fA bij total" (Total.modify (mulDiv `iso` fA) (* 0.5) tot2) tot3
  , eq "modify psnd bij inv total" (Total.modify (inv mulDiv `iso` psnd) (`div` 2) snd1) snd0
  ]

applicative :: Test
applicative = TestList
  [ eq "get tA appl" (Total.get (tA . totTot2) tot0) Nothing
  , eq "get tB appl" (Total.get (tB . totTot2) tot0) (Left 1)
  , eq "get tD appl" (Total.get (tD . totTot2) tot0) nt0
  , eq "set tA appl" (Total.set (tA . totTot2) (Just nt1) tot1) tot4
  , eq "set tA appl" (Total.set (tA . totTot2) Nothing tot4) tot3
  , eq "mod tA appl" (Total.modify (tB . totTot2) (fmap not) tot6) tot7
  ]

eq :: (Eq a, Show a) => String -> a -> a -> Test
eq d a b = TestCase (assertEqual d a b)

