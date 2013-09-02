{-# LANGUAGE TemplateHaskell #-}

import Data.Label
import Prelude hiding ((.), id)
import Control.Category
import Criterion.Main

data Person = Person
  { _name   :: String
  , _age    :: Int
  , _place  :: Place
  , _birthplace :: Maybe Place
  } deriving (Show, Eq)

data Place = Place
  { _city
  , _country
  , _continent :: String
  } deriving (Show, Eq)

mkLabels [''Person, ''Place]

jan :: Person
jan = Person "Jan" 71 (Place "Utrecht" "The Netherlands" "Europe") Nothing

getAge :: Int
getAge = get age jan

moveToAmsterdam :: Person -> Person
moveToAmsterdam = set (city . place) "Amsterdam"

moveToAmsterdam' :: Person -> Person
moveToAmsterdam' person = person{_place = (_place person){_city = "Amsterdam"}}

ageByOneYear :: Person -> Person
ageByOneYear = modify age (+1)

ageByOneYear' :: Person -> Person
ageByOneYear' person = person{_age = (+1) $ _age person}

moveAndAge :: Person -> Person
moveAndAge = ageByOneYear . moveToAmsterdam . ageByOneYear . ageByOneYear . ageByOneYear

moveAndAge' :: Person -> Person
moveAndAge' = ageByOneYear' . moveToAmsterdam' . ageByOneYear' . ageByOneYear' . ageByOneYear'

main :: IO ()
main = 
  defaultMain 
    [ bench "warmup" $ whnf show "Hello World"
    , bench "ageByOneYear" $ whnf ageByOneYear jan
    , bench "ageByOneYear'" $ whnf ageByOneYear' jan
    , bench "moveToAmsterdam" $ whnf moveToAmsterdam jan
    , bench "moveToAmsterdam'" $ whnf moveToAmsterdam' jan
    , bench "moveAndAge" $ whnf moveAndAge jan
    , bench "moveAndAge'" $ whnf moveAndAge' jan
    ]

