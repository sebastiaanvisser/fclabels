{-# LANGUAGE
    TemplateHaskell
  , Arrows
  , TypeOperators
  #-}
module TestArrow where

import Control.Arrow
import Control.Category
import Data.Record.Label.Pure
import Prelude hiding ((.), id)
import qualified Data.Record.Label.Abstract as Abs

-------------------------------------------------------------------------------
-- | Generic Maybe label, embedding in the maybe zero.

maybeL :: (ArrowZero (~>), ArrowChoice (~>)) => Lens (~>) (Maybe a) a
maybeL = Abs.lens getter setter
  where
    getter = proc p ->
      do case p of
           Just n  -> returnA   -< n
           Nothing -> zeroArrow -< p
    setter = proc (v, p) ->
      do case p of
           Just _  -> returnA -< Just v
           Nothing -> returnA -< p

-------------------------------------------------------------------------------

data Twofold a b =
    Person 
      { name   :: String
      , email  :: Int
      , spouse :: Maybe (Twofold b a)
      }
  | Animal
      { kind   :: String
      , legs   :: Int
      }

nameL :: (ArrowZero (~>), ArrowChoice (~>)) => Lens (~>) (Twofold a b) String
nameL = Abs.lens getter setter
  where
    getter = proc p ->
      do case p of
           Person n _ _ -> returnA   -< n
           Animal {}    -> zeroArrow -< p
    setter = proc (v, p) ->
      do case p of
           Person _ e s -> returnA -< Person v e s
           Animal {}    -> returnA -< p

spouseL :: (ArrowChoice (~>), ArrowZero (~>), ArrowApply (~>)) => Lens (~>) (Twofold a b) (Twofold b a)
spouseL = maybeL . Abs.lens getter setter
  where
    getter = proc p ->
      do case p of
           Person _ _ s -> returnA   -< s
           Animal {}    -> zeroArrow -< p
    setter = proc (v, p) ->
      do case p of
           Person n e _ -> returnA -< Person n e v
           Animal {}    -> returnA -< p

-------------------------------------------------------------------------------

data Single =
    Single 
      { sName  :: String
      , sEmail :: Integer
      }

sNameL :: Arrow (~>) => Lens (~>) Single String
sNameL = Abs.lens getter setter
  where
    getter = arr (\(Single n _) -> n)
    setter = arr (\(v, Single _ e) -> Single v e)

sEmailL :: Arrow (~>) => Lens (~>) Single Integer
sEmailL = Abs.lens getter setter
  where
    getter = arr (\(Single _ e) -> e)
    setter = arr (\(v, Single n _) -> Single n v)

-------------------------------------------------------------------------------

nameSpouseSpouse :: Twofold a b -> Maybe String
nameSpouseSpouse = getLM (nameL . spouseL . spouseL)

-- nameSpouseSpouse = getL (nameL . spouseL . spouseL)
-- Produces: No instance for (ArrowZero (->))

theName :: Single -> String
theName = getL sNameL

theNameM :: Single -> Maybe String
theNameM = getLM sNameL

