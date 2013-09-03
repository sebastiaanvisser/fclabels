{- | Template Haskell functions for automatically generating labels for
algebraic datatypes, newtypes and GADTs.
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
) where

import Control.Arrow
import Control.Category
import Control.Monad
import Data.Char
import Data.Function (on)
import Data.Label.Abstract
import Data.Label.Total ((:->))
import Data.Label.Partial ((:~>))
import Data.List
import Data.Ord
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding ((.), id)

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
    let (tyname, cons, vars) =
          case info of
            TyConI (DataD    _ n vs cs _) -> (n, cs,  vs)
            TyConI (NewtypeD _ n vs c  _) -> (n, [c], vs)
            _ -> fclError "Can only derive labels for datatypes and newtypes."

        -- We are only interested in lenses of record constructors.
        recordOnly = groupByCtor [ (f, n) | RecC n fs <- cons, f <- fs ]

    concat `liftM` mapM (derive mk sigs conc tyname vars (length cons)) recordOnly

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
derive mk sigs conc tyname vars total ((field, _, fieldtyp), ctors) =

  do (sign, body) <-
       if length ctors == total
       then function mkTotal
       else function mkPartial

     return $
       if sigs
       then [sign, inline, body]
       else [inline, body]

  where

    -- Generate an inline declaration for the label.
    --
    -- Type of InlineSpec removed in TH-2.8.0 (GHC 7.6)
#if MIN_VERSION_template_haskell(2,8,0)
    inline = PragmaD (InlineP label Inline FunLike (FromPhase 0))
#else
    inline = PragmaD (InlineP label (InlineSpec True True (Just (True, 0))))
#endif
    name  = mk (nameBase field)
    label = mkName name

    ---------------------------------------------------------------------------
    -- Build a single record label definition for labels that might fail.
    mkPartial = (if conc then mono else poly, body)
      where
        mono = forallT prettyVars (return [])
                 [t| $(inputType) :~> $(return prettyFieldtyp) |]
        poly = forallT foralls (return [])
                 [t| (ArrowChoice $(cat), ArrowFail String $(cat), ArrowApply $(cat))
                   => Lens $(cat) $(inputType) $(return prettyFieldtyp) |]
        body = [| lens (failArrow ||| $(totalG) <<< $(caseG))
                       (failArrow ||| $(totalM) <<< $(caseM))
                |]
          where
            bodyG f   = [| Right $(f) |]
            cases b   = map (\ctor -> match (recP ctor []) (normalB b) []) ctors
            wild      = [match wildP (normalB failure) []]
            failure   = [| Left $(litE (stringL name) `sigE` [t| String |] ) |]
            caseG     = [| arr (\f      -> $(caseE [|f|] (cases (bodyG [|f     |]) ++ wild))) |]
            caseM     = [| arr (\(m, f) -> $(caseE [|f|] (cases (bodyG [|(m, f)|]) ++ wild))) |]

    ---------------------------------------------------------------------------
    -- Build a single record label definition for labels that cannot fail.
    mkTotal = (if conc then mono else poly, body)
      where
        mono = forallT prettyVars (return [])
                 [t| $(inputType) :-> $(return prettyFieldtyp) |]
        poly = forallT foralls (return [])
                 [t| ArrowApply $(cat) => Lens $(cat) $(inputType) $(return prettyFieldtyp) |]
        body = [| lens $(totalG) $(totalM) |]

    ---------------------------------------------------------------------------
    -- The total getter and setter arrows. Directly used by total lenses,
    -- indirectly used by partial lenses.
    totalG = [| arr $(varE field) |]
    totalM = [| modifier $(varE field) (\(v, f) -> $(record [| f |] field [| v |])) |]

    -- Compute the type (including type variables of the record datatype.
    inputType = return $ foldr (flip AppT)
                               (ConT tyname)
                               (map tvToVarT (reverse prettyVars))

    -- Convert a type variable binder to a regular type variable.
    tvToVarT (PlainTV  tv     ) = VarT tv
    tvToVarT (KindedTV tv kind) = SigT (VarT tv) kind

    -- Prettify type variables.
    cat            = varT (mkName "cat")
    prettyVars     = map prettyTyVar vars
    foralls        = PlainTV (mkName "cat") : prettyVars
    prettyFieldtyp = prettyType fieldtyp

    -- Q style record updating.
    record rec fld val = val >>= \v -> recUpdE rec [return (fld, v)]

    -- Build a function declaration with both a type signature and body.
    function (s, b) = liftM2 (,) 
        (sigD label s)
        (funD label [ clause [] (normalB b) [] ])

-- Build a total modification function from a pure getter and setter.

modifier :: ArrowApply cat => (f -> a) -> ((a, f) -> f) -> cat (cat a a, f) f
modifier pg pm
  = arr pm
  . first app
  . arr (\(m, (f, o)) -> ((m, o), f))
  . second (id &&& arr pg)

-------------------------------------------------------------------------------
-- Cleaning up names in generated code.

-- Prettify a TH name.

pretty :: Name -> Name
pretty tv = mkName (takeWhile (/='_') (show tv))

-- Prettify a type variable by prettyfing all names.

prettyTyVar :: TyVarBndr -> TyVarBndr
prettyTyVar (PlainTV  tv   ) = PlainTV (pretty tv)
prettyTyVar (KindedTV tv ki) = KindedTV (pretty tv) ki

-- Prettify a type predicate.

prettyPred :: Pred -> Pred
prettyPred (ClassP nm tys) = ClassP (pretty nm) (map prettyType tys)
prettyPred (EqualP ty tx ) = EqualP (prettyType ty) (prettyType tx)

-- Prettify a type.

prettyType :: Type -> Type
prettyType (ForallT xs cx ty) = ForallT (map prettyTyVar xs) (map prettyPred cx) (prettyType ty)
prettyType (VarT nm         ) = VarT (pretty nm)
prettyType (AppT ty tx      ) = AppT (prettyType ty) (prettyType tx)
prettyType (SigT ty ki      ) = SigT (prettyType ty) ki
prettyType ty                 = ty

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

