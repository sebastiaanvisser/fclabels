{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude hiding ((.), id, mod)
import Control.Category
import Control.Applicative
import Data.Record.Label

data Person = Person
  { _name   :: String
  , _age    :: Int
  , _isMale :: Bool
  , _place  :: Place
  } deriving Show

data Place = Place
  { _city
  , _country
  , _continent :: String
  } deriving Show
  
$(mkLabels [''Person, ''Place])

name      :: Person :-> String
age       :: Person :-> Int
isMale    :: Person :-> Bool
place     :: Person :-> Place
city      :: Place :-> String
country   :: Place :-> String
continent :: Place :-> String

testUser :: Person
testUser = Person "sebas" 26 True (Place "Utrecht" "The Netherlands" "Europe")

ageAndCity :: Person :-> (Int, String)
ageAndCity = Label $ (,) <$> fst `for` age <*> snd `for` (city . place)

