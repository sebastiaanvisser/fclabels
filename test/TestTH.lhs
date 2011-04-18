Run this file with 'ghci -ddump-splices TestTH.lhs'

> {-# LANGUAGE TemplateHaskell #-}

> module TestTH where

> import Data.Label
> import qualified Data.Label.Maybe as M

> data Person a b =
>   Person
>    { _name   :: String
>    , _email  :: Int
>    , _ann    :: [Person b a]
>    }
>  | NoPerson { _non :: (), _ann :: [Person b a] }
>  deriving Show

> $(mkLabels [''Person])

> myPerson :: Person b b
> myPerson = Person "NAME" 3 [myPerson]


