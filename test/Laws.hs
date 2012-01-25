{-# LANGUAGE TypeOperators, GeneralizedNewtypeDeriving #-}
module Laws where

import Prelude hiding ((.), id)
import Control.Category
import Data.Label
import Test.QuickCheck

newtype K = K { x :: Int }
  deriving (Arbitrary, Show, Eq)

myLens :: K :-> Int
myLens = lens x (\y f -> f { x = y })

myId :: K :-> K
myId = id

main :: IO ()
main =
  do quickCheck (getSet myLens)
     quickCheck (setGet myLens)
     quickCheck (getSet myId)
     quickCheck (setGet myId)

-- The get after set law:

getSet :: Eq a => (f :-> a) -> f -> a -> Bool
getSet l f a = get l (set l a f) == a

-- The set after get law:

setGet :: Eq f => (f :-> a) -> f -> Bool
setGet l f = set l (get l f) f == f

-- It's fairly easy to prove these laws equivalent.
--
-- set l (get l f) == f
-- 
--   Add (get l) to both sides:
--
-- get l (set l (get l f)) == get l f
--
--   Substitute (get l f) for a
--
-- get l (set l a f) == a
-- 
--   QED.

