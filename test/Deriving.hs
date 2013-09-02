-- {-# OPTIONS -ddump-splices #-}
{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeOperators #-}

module TestTH where

import Control.Category
import Data.Label
import Data.Maybe
import Data.String
import Prelude hiding ((.), id)
import qualified Data.Label.Either as E
import qualified Data.Label.Maybe as M

data Pet = Cat | Dog
  deriving Show

data Family a = Family
  { _father :: Maybe (Member a)
  , _mother :: Either String (Member a)
  , _child  :: Member a
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

mkLabels [''Family, ''Member]

myself :: Member Int
myself = Person "Me" 28 (Family (Just myDad) (Right myMum) myself)

myMum :: Member Int
myMum = Person "Mum" 53 (Family Nothing (Left "myMum") myMum)

myDad :: Member Int
myDad = Person "Dad" 55 (Family Nothing (Left "myDad") myDad)

cat :: Member Int
cat = Pet Cat "Puss" 8

-------------------------------------------------------------------------------

howOldIsMum :: Int
howOldIsMum = get age myMum

animalKind :: Maybe Pet
animalKind = M.get kind cat

humanKind :: Either String Pet
humanKind = E.get kind myDad

myFathersBirthday :: Maybe (Member Int)
myFathersBirthday = M.modify (age . M.embed father . family) (+ 1) myself

myMothersBirthday :: Either String (Member Int)
myMothersBirthday = E.modify (age . E.embed mother . family) (+ 1) myself

test1 :: Lens M.Partial (Member Int) (Member Int)
test1 = child . family . child . family

test2 :: Lens (E.Failure String) (Member Int) (Member Int)
test2 = child . family . child . family

