{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import Prelude hiding ((.), id)
import Data.Label

data D = A { _a :: () } | B { _b :: Char }

$(mkLabels [''D])

