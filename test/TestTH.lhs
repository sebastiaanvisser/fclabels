Run this file with 'ghci -ddump-splices TestTH.lhs'

> {-# LANGUAGE TemplateHaskell #-}

> module TestTH where

> import Control.Arrow
> import Data.Label

> data Person a b =
>   Person
>    { _name   :: String
>    , _email  :: Int
>    , _ann    :: [Person b a]
>    }
>  | NoPerson { _non :: (), _ann :: [Person b a] }

> $(mkLabels [''Person])

