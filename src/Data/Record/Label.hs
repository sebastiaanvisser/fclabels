module Data.Record.Label
(
-- * Lens types.
  Point (Point)
, (:->) (Lens)
, lens
, getL, setL, modL

, fmapL

-- * Bidirectional functor.
, (:<->:) (..)
, Iso (..)
, lmap
, for

-- * Monadic lens operations.
, getM, setM, modM, (=:)
, askM, localM

-- * Derive labels using Template Haskell.
, module Data.Record.Label.TH
)
where

import Data.Record.Label.Core
import Data.Record.Label.Monadic
import Data.Record.Label.TH
