-- {-# OPTIONS -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}

module TestTH where

import Control.Category
import Data.Label
import Data.Maybe
import Prelude hiding ((.), id)
import qualified Data.Label.Maybe as M

data Pet = Cat | Dog
  deriving Show

data Family a = Family
  { _father :: Maybe (Member a)
  , _mother :: Maybe (Member a)
  }
 deriving Show

data Member a =
  Person
   { _name   :: String
   , _age    :: a
   , _family :: Family a
   }
 | Pet
   { _kind   :: Pet
   , _name   :: String
   , _age    :: a
   }
 deriving Show

$(mkLabels [''Family, ''Member])

myself :: Member Int
myself = Person "Me" 28 (Family (Just myDad) (Just myMum))

myMum :: Member Int
myMum = Person "Mum" 53 (Family Nothing Nothing)

myDad :: Member Int
myDad = Person "Dad" 55 (Family Nothing Nothing)

cat :: Member Int
cat = Pet Cat "Puss" 8

-------------------------------------------------------------------------------

howOldIsMum :: Int
howOldIsMum = get age myMum

animalKind :: Maybe Pet
animalKind = M.get kind cat

myFathersBirthday :: Maybe (Member Int)
myFathersBirthday = M.modify (age . M.embed father . family) (+ 1) myself

