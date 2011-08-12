{-# LANGUAGE
    TemplateHaskell
  , Arrows
  , TypeOperators
  #-}
module TestArrow where

import Control.Arrow
import Control.Category
import Data.Label.Abstract (Lens)
import Data.Label.Pure
import Prelude hiding ((.), id)
import qualified Data.Label.Abstract as Abs
import qualified Data.Label.Maybe as M

-------------------------------------------------------------------------------
-- | Generic Maybe label, embedding in the maybe zero.

-- To be derived using TH:

maybeL :: (ArrowZero (~>), ArrowChoice (~>)) => Lens (~>) (Maybe a) a
maybeL = Abs.lens getter setter
  where getter = proc p ->
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
      { name  :: String
      , email :: Int
      , other :: Maybe (Twofold b a)
      }
  | Animal
      { kind  :: String
      , legs  :: Int
      }
  | Robot
      { name  :: String
      }


-- To be derived using TH:

{-
nameL :: (ArrowZero (~>), ArrowChoice (~>)) => Lens (~>) (Twofold a b) String
nameL = Abs.lens getter setter
  where getter = proc p ->
          do case p of
               Person n _ _ -> returnA   -< n
               Robot  n     -> returnA   -< n
               _            -> zeroArrow -< ()
        setter = proc (v, p) ->
          do case p of
               Person {}    -> returnA -< p { name = v }
               Robot  {}    -> returnA -< p { name = v }
               _            -> zeroArrow -< ()
-}

nameL :: (ArrowChoice (~>), ArrowZero (~>)) => Lens (~>) (Twofold a b) String
nameL =
  Abs.lens
    (arr (\p -> case p of
                   Person {} -> Right (name p)
                   Robot  {} -> Right (name p)
                   _         -> Left ()
         ) >>> (zeroArrow ||| returnA))
    (arr (\(v, p) ->
       case p of
          Person {} -> Right p { name = v }
          Robot  {} -> Right p { name = v }
          _         -> Left ()
    ) >>> (zeroArrow ||| returnA))


otherL :: (ArrowChoice (~>), ArrowZero (~>), ArrowApply (~>)) => Lens (~>) (Twofold a b) (Twofold b a)
otherL = maybeL . Abs.lens getter setter
  where getter = proc p ->
          do case p of
               Person _ _ s -> returnA   -< s
               _            -> zeroArrow -< p
        setter = proc (v, p) ->
          do case p of
               Person n e _ -> returnA -< Person n e v
               _            -> returnA -< p

-------------------------------------------------------------------------------

data Single = Single 
  { sName  :: String
  , sEmail :: Integer
  }

-- To be derived using TH:

sNameL :: Arrow (~>) => Lens (~>) Single String
sNameL = Abs.lens getter setter
  where getter = arr (\(Single n _) -> n)
        setter = arr (\(v, Single _ e) -> Single v e)

sEmailL :: Arrow (~>) => Lens (~>) Single Integer
sEmailL = Abs.lens getter setter
  where getter = arr (\(Single _ e) -> e)
        setter = arr (\(v, Single n _) -> Single n v)

-------------------------------------------------------------------------------

-- Lenses that cannot fail are safely runnable using both the function and the
-- maybe context.

theName :: Single -> String
theName = get sNameL

theNameM :: Single -> Maybe String
theNameM = M.get sNameL

-- Lenses that might fail are safely runnable in the maybe context, but
-- produce a type error when used in the pure function context.

nameOtherOther :: Twofold a b -> Maybe String
nameOtherOther = M.get (nameL . otherL . otherL)

-- nameOtherOther = getL (nameL . otherL . otherL)
--    Produces: No instance for (ArrowZero (->))

