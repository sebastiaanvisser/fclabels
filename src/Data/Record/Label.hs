module Data.Record.Label
(
-- * Lens types.
  Point (Point)
, Lens (Lens)
, (:->)
, lens
, getL, setL, modL

-- * Bidirectional functor.
, Bijection (..)
, Iso (..)
, for

-- * Monadic lens operations.
, getM, setM, modM, (=:)
, askM, localM

-- * Derive labels using Template Haskell.
, module Data.Record.Label.TH
)
where

import Data.Record.Label.Pure
import Data.Record.Label.Monadic
import Data.Record.Label.TH

