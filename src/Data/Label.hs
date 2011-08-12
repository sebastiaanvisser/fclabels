{-# LANGUAGE TypeOperators #-}
module Data.Label
(

-- * Pure, non-failing lenses.
  (:->)
, lens
, get
, set
, Pure.mod

-- * Working with bijections and isomorphisms.
, Bijection (..)
, Iso (..)
, for

-- * Derive labels using Template Haskell.
, mkLabels
, mkLabelsNoTypes
)
where

import Data.Label.Abstract (Bijection(..), Iso(..), for)
import Data.Label.Pure
import Data.Label.Derive
import qualified Data.Label.Pure as Pure

