Run this file with 'ghci -ddump-splices TestTH.lhs'

> {-# LANGUAGE TemplateHaskell #-}
 
> module TestTH where
  
> import Data.Label
 
> data Person a b = Person 
>     { name   :: String
>     , email  :: Int
>     , ann    :: Maybe (Person b a)
>     }

> $(mkLabels [''Person])
