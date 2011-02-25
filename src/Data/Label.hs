module Data.Label
(

-- * Pure, non-failing lenses.
  (:->)
, lens
, get
, set
, mod

-- * Working with bijections and isomorphisms.
, Bijection (..)
, Iso (..)
, for

-- * Derive labels using Template Haskell.
, module Data.Label.TH
)
where

import Data.Label.Abstract (Bijection(..), Iso(..), for)
-- import Data.Label.Maybe
import Data.Label.Pure
import Data.Label.TH
import Prelude hiding (mod)

-- Lenses that might fail.
-- , (:~>)
-- , lensM
-- , getLM
-- , setLM
-- , modLM

-- Monadic operations for pure lenses.
-- , getM
-- , setM
-- , modM
-- , (=:)
-- , askM
-- , localM

-- Monadic operations for lenses that might fail.
-- , getMP
-- , askMP

