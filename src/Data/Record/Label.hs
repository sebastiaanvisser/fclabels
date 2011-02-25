module Data.Record.Label
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
, module Data.Record.Label.TH
)
where

import Data.Record.Label.Abstract (Bijection(..), Iso(..), for)
-- import Data.Record.Label.Maybe
import Data.Record.Label.Pure
import Data.Record.Label.TH
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

