module Data.Record.Label
(

-- * Pure non-failing lenses.
  (:->)
, lens
, getL
, setL
, modL

-- * Lenses that might fail.
, (:~>)
, lensM
, getLM
, setLM
, modLM

-- * Working with bijections and isomorphisms.
, Bijection (..)
, Iso (..)
, for

-- * Monadic operations for pure lenses.
, getM, setM, modM, (=:)
, askM, localM

-- * Derive labels using Template Haskell.
, module Data.Record.Label.TH
)
where

import Data.Record.Label.Abstract (Bijection(..), Iso(..), for)
import Data.Record.Label.Monadic
import Data.Record.Label.Pure
import Data.Record.Label.TH

