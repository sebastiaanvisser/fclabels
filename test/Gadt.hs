{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

import Prelude hiding ((.), id)
import Data.Label
import qualified Data.Label.Maybe as M

data D a where
  A :: { _a :: () } -> D ()
  B :: { _b :: a } -> D a

deriving instance Show a => Show (D a)

$(mkLabels [''D])

x = M.set b 'd' (B 'c')
