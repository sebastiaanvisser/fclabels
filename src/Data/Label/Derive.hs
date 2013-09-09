{- |
Template Haskell functions for automatically generating labels for algebraic
datatypes, newtypes and GADTs.
-}

{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    TemplateHaskell
  , OverloadedStrings
  , FlexibleContexts
  , FlexibleInstances
  , TypeOperators
  , CPP #-}

module Data.Label.Derive
( mkLabel
, mkLabels
, mkLabelsWith
, mkLabelsMono
, mkLabelsNoTypes
, deriveWith
, defaultMakeLabel
)
where

import Control.Arrow
import Control.Applicative
import Control.Category
import Control.Monad
import Data.Char (toLower, toUpper)
import Data.Function (on)
import Data.Label.Partial ((:~>))
import Data.Label.Point hiding (id)
import Data.Label.Total ((:->))
import Data.List (groupBy, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding ((.), id)

import qualified Data.Label.Mono as Mono
import qualified Data.Label.Poly as Poly

-- | Derive lenses including type signatures for all the record selectors for a
-- collection of datatypes. The types will be polymorphic and can be used in an
-- arbitrary context.

mkLabels :: [Name] -> Q [Dec]
mkLabels = mkLabelsWith defaultMakeLabel

-- | Derive lenses including type signatures for all the record selectors in a
-- single datatype. The types will be polymorphic and can be used in an
-- arbitrary context.

mkLabel :: Name -> Q [Dec]
mkLabel = mkLabels . return

-- | Like `mkLabels`, but uses the specified function to produce custom names
-- for the labels.
--
-- For instance, @(drop 1 . dropWhile (/='_'))@ creates a label
-- @val@ from a record @Rec { rec_val :: X }@.

mkLabelsWith :: (String -> String) -> [Name] -> Q [Dec]
mkLabelsWith mk = liftM concat . mapM (reify >=> deriveWith mk True False)

-- | Derive lenses including type signatures for all the record selectors in a
-- datatype. The signatures will be concrete and can only be used in the
-- appropriate context.

mkLabelsMono :: [Name] -> Q [Dec]
mkLabelsMono = liftM concat . mapM (reify >=> deriveWith defaultMakeLabel True True)

-- | Derive lenses without type signatures for all the record selectors in a
-- datatype.

mkLabelsNoTypes :: [Name] -> Q [Dec]
mkLabelsNoTypes = liftM concat . mapM (reify >=> deriveWith defaultMakeLabel False False)

-- | Low level label generation function.

deriveWith :: (String -> String) -- ^ Supply a function to perform custom label naming.
           -> Bool               -- ^ Generate type signatures or not.
           -> Bool               -- ^ Generate concrete type or abstract type.
           -> Info               -- ^ The type to derive labels for.
           -> Q [Dec]
deriveWith mk sigs conc info =

 do -- Only process data and newtype declarations, filter out all
    -- constructors and the type variables.
    let (tyname, cons, tyvars) =
          case info of
            TyConI (DataD    _ n vs cs _) -> (n, cs,  vs)
            TyConI (NewtypeD _ n vs c  _) -> (n, [c], vs)
            _ -> fclError "Can only derive labels for datatypes and newtypes."

        -- We are only interested in lenses of record constructors.
        recordOnly = groupByCtor [ (f, n) | RecC n fs <- cons, f <- fs ]

    concat `liftM` mapM (derive mk sigs conc tyname tyvars (length cons)) recordOnly

    where groupByCtor = map (\xs -> (fst (head xs), map snd xs))
                      . groupBy ((==) `on` (fst3 . fst))
                      . sortBy (comparing (fst3 . fst))
                      where fst3 (a, _, _) = a

-------------------------------------------------------------------------------
-- Generate the code for the labels.

-- | Generate a name for the label. If the original selector starts with an
-- underscore, remove it and make the next character lowercase. Otherwise, add
-- 'l', and make the next character uppercase.

defaultMakeLabel :: String -> String
defaultMakeLabel field =
  case field of
    '_' : c : rest -> toLower c : rest
    f : rest       -> 'l' : toUpper f : rest
    n              -> fclError ("Cannot derive label for record selector with name: " ++ n)

derive :: (String -> String)
       -> Bool -> Bool -> Name -> [TyVarBndr] -> Int
       -> (VarStrictType, [Name]) -> Q [Dec]
derive mk sigs conc tyname tyvars total ((field, _, fieldtyp), ctors) =

     -- Compute the type (including type variables of the record datatype.
  do (inTy, outTy, vars, typ, ctor) <- computeType fieldtyp tyname tyvars

     let cat    = varT (mkName "cat")
         catTv  = PlainTV (mkName "cat")
         forall = forallT (catTv : vars) (return [])

         -- Q style record updating.
         record rec fld val = val >>= \v -> recUpdE rec [return (fld, v)]

         -- The label name.
         name  = mk (nameBase field)
         label = mkName name

         -- Build a function declaration with both a type signature and body.
         function (s, b) = liftM2 (,) 
             (sigD label s)
             (funD label [ clause [] (normalB b) [] ])

#if MIN_VERSION_template_haskell(2,8,0)
         -- Generate an inline declaration for the label.
         -- Type of InlineSpec removed in TH-2.8.0 (GHC 7.6)
         inline = PragmaD (InlineP label Inline FunLike (FromPhase 0))
#else
         inline = PragmaD (InlineP label (InlineSpec True True (Just (True, 0))))
#endif

         -- The total getter and setter arrows. Directly used by total lenses,
         -- indirectly used by partial lenses.
         totalG = [| arr $(varE field) |]
         totalM = [| modifier $(varE field) (\(v, f) -> $(record [| f |] field [| v |])) |]

         ---------------------------------------------------------------------------
         -- Build a single record label definition for labels that might fail.

         mkPartialPoint = (if conc then mono else poly, body)
           where
             mono = forall [t| $(inTy) :~> $(outTy) |]
             poly = forall [t| (ArrowChoice $(cat), ArrowFail String $(cat), ArrowApply $(cat))
                        => $(typ) $(cat) $(inTy) $(outTy) |]
             body = [| $(ctor)
                         $ Point (failArrow ||| $(totalG) <<< $(caseG))
                                 (failArrow ||| $(totalM) <<< $(caseM))
                     |]
             bodyG f = [| Right $(f) |]
             cases b = map (\c -> match (recP c []) (normalB b) []) ctors
             wild    = [match wildP (normalB failure) []]
             failure = [| Left $(litE (stringL name) `sigE` [t| String |] ) |]
             caseG   = [| arr (\f      -> $(caseE [|f|] (cases (bodyG [|f     |]) ++ wild))) |]
             caseM   = [| arr (\(m, f) -> $(caseE [|f|] (cases (bodyG [|(m, f)|]) ++ wild))) |]

         ---------------------------------------------------------------------------
         -- Build a single record label definition for labels that cannot fail.

         mkTotalPoint = (if conc then mono else poly, body)
           where
             mono = forall [t| $(inTy) :-> $(outTy) |]
             poly = forall [t| ArrowApply $(cat) => $(typ) $(cat) $(inTy) $(outTy) |]
             body = [| $(ctor) (Point $(totalG) $(totalM)) |]

     (sign, body) <-
       if length ctors == total
       then function mkTotalPoint
       else function mkPartialPoint

     return $ if sigs
              then [sign, inline, body]
              else [      inline, body]

computeType :: Type -> Name -> [TyVarBndr] -> Q (TypeQ, TypeQ, [TyVarBndr], TypeQ, ExpQ)
computeType field datatype vars =
  do let fieldVars = typeVariables field
         outTy     = return field
         varNames  = fromTyVarBndr <$> vars
         usedVars  = filter (`elem` fieldVars) varNames
         inTy      = foldr (flip appT) (conT datatype)
                           (return . tvToVarT <$> reverse vars)

     case usedVars of
       [] -> return
         ( prettyType <$> inTy
         , prettyType <$> outTy
         , mapTyVarBndr pretty <$> vars
         , [t| Mono.Lens |]
         ,  [| Poly.Lens |]
         )
       vs ->
         do let names = return <$> ['a'..'z']
                used  = show . pretty <$> varNames
                free  = filter (not . (`elem` used)) names
            subs <- forM (zip vs free) (\(a, b) -> (,) a <$> newName b)
            let rename = mapTypeVariables (\a -> a `fromMaybe` lookup a subs)

            return
              ( prettyType <$> [t| $(inTy)  -> $(rename <$> inTy)  |]
              , prettyType <$> [t| $(outTy) -> $(rename <$> outTy) |]
              , mapTyVarBndr pretty <$> (vars ++ (PlainTV . snd <$> subs))
              , [t| Poly.Lens |]
              ,  [| Poly.Lens |]
              )

  where -- Convert a binder to a regular type variable.
        tvToVarT (PlainTV  tv     ) = VarT tv
        tvToVarT (KindedTV tv kind) = SigT (VarT tv) kind

-- Build a total polymorphic modification function from a pure getter and setter.

modifier :: ArrowApply cat => (f -> o) -> ((i, f) -> g) -> cat (cat o i, f) g
modifier pg pm
  = arr pm
  . first app
  . arr (\(m, (f, o)) -> ((m, o), f))
  . second (id &&& arr pg)
{-# INLINE modifier #-}

-------------------------------------------------------------------------------

-- Compute all the free type variables from a type.

typeVariables :: Type -> [Name]
typeVariables ty =
  case ty of
    ForallT ts _ _ -> fromTyVarBndr <$> ts
    AppT a b       -> typeVariables a ++ typeVariables b
    SigT t _       -> typeVariables t
    VarT n         -> [n]
    _              -> []

mapTypeVariables :: (Name -> Name) -> Type -> Type
mapTypeVariables f ty =
  case ty of
    ForallT ts a b -> ForallT (mapTyVarBndr f <$> ts) (mapPred f <$> a) (mapTypeVariables f b)
    AppT a b       -> AppT (mapTypeVariables f a) (mapTypeVariables f b)
    SigT t a       -> SigT (mapTypeVariables f t) a
    VarT n         -> VarT (f n)
    t              -> t

fromTyVarBndr :: TyVarBndr -> Name
fromTyVarBndr (PlainTV  n  ) = n
fromTyVarBndr (KindedTV n _) = n

mapPred :: (Name -> Name) -> Pred -> Pred
mapPred f (ClassP n ts) = ClassP (f n) (mapTypeVariables f <$> ts)
mapPred f (EqualP t x ) = EqualP (mapTypeVariables f t) (mapTypeVariables f x)

mapTyVarBndr :: (Name -> Name) -> TyVarBndr -> TyVarBndr
mapTyVarBndr f (PlainTV  n  ) = PlainTV (f n)
mapTyVarBndr f (KindedTV n a) = KindedTV (f n) a

-- Prettify a TH name.

pretty :: Name -> Name
pretty tv = mkName (takeWhile (/= '_') (show tv))

-- Prettify a type.

prettyType :: Type -> Type
prettyType = mapTypeVariables pretty

-- IsString instances for TH types.

instance IsString Exp where
  fromString = VarE . mkName

instance IsString (Q Pat) where
  fromString = varP . mkName

instance IsString (Q Exp) where
  fromString = varE . mkName

-- Throw a fclabels specific error.

fclError :: String -> a
fclError err = error ("Data.Label.Derive: " ++ err)

