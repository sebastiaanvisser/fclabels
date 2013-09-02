{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE
    TemplateHaskell
  , OverloadedStrings
  , FlexibleContexts
  , FlexibleInstances
  , TypeOperators
  , CPP #-}
module Data.Label.Derive
( mkLabels
, mkLabel
, mkLabelsWith
, mkLabelsMono
, mkLabelsNoTypes
, defaultMakeLabel
, gDerive
) where

import Control.Arrow
import Control.Category
import Control.Monad
import Data.Char
import Data.Function (on)
import Data.Label.Abstract
import Data.Label.Total ((:->))
import Data.Label.Maybe ((:~>))
import Data.List
import Data.Ord
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding ((.), id)

-- Throw a fclabels specific error.

fclError :: String -> a
fclError err = error ("Data.Label.Derive: " ++ err)

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

-- | Generate the label name from the record field name.
-- For instance, @drop 1 . dropWhile (/='_')@ creates a label @val@ from a
-- record @Rec { rec_val :: X }@.

mkLabelsWith :: (String -> String) -> [Name] -> Q [Dec]
mkLabelsWith mk = liftM concat . mapM (derive1 mk True False)

-- | Derive lenses including type signatures for all the record selectors in a
-- datatype. The signatures will be concrete and can only be used in the
-- appropriate context.

mkLabelsMono :: [Name] -> Q [Dec]
mkLabelsMono = liftM concat . mapM (derive1 defaultMakeLabel True True)

-- | Derive lenses without type signatures for all the record selectors in a
-- datatype.

mkLabelsNoTypes :: [Name] -> Q [Dec]
mkLabelsNoTypes = liftM concat . mapM (derive1 defaultMakeLabel False False)

-- Helpers to generate all labels for one datatype.

derive1 :: (String -> String) -> Bool -> Bool -> Name -> Q [Dec]
derive1 mk signatures concrete = reify >=> gDerive mk signatures concrete

gDerive :: (String -> String) -> Bool -> Bool -> Info -> Q [Dec]
gDerive mk signatures concrete i =
 do let -- Only process data and newtype declarations, filter out all
        -- constructors and the type variables.
        (tyname, cons, vars) =
          case i of
            TyConI (DataD    _ n vs cs _) -> (n, cs,  vs)
            TyConI (NewtypeD _ n vs c  _) -> (n, [c], vs)
            _                             -> fclError "Can only derive labels for datatypes and newtypes."

        -- We are only interested in lenses of record constructors.
        recordOnly = groupByCtor [ (f, n) | RecC n fs <- cons, f <- fs ]

    concat `liftM`
        mapM (derive mk signatures concrete tyname vars (length cons))
            recordOnly

    where groupByCtor = map (\xs -> (fst (head xs), map snd xs))
                      . groupBy ((==) `on` (fst3 . fst))
                      . sortBy (comparing (fst3 . fst))
                      where fst3 (a, _, _) = a

-- Generate the code for the labels.

-- | Generate a name for the label. If the original selector starts with an
-- underscore, remove it and make the next character lowercase. Otherwise,
-- add 'l', and make the next character uppercase.
defaultMakeLabel :: String -> String
defaultMakeLabel field =
  case field of
    '_' : c : rest -> toLower c : rest
    f : rest       -> 'l' : toUpper f : rest
    n              -> fclError ("Cannot derive label for record selector with name: " ++ n)

derive :: (String -> String)
       -> Bool -> Bool -> Name -> [TyVarBndr] -> Int
       -> (VarStrictType, [Name]) -> Q [Dec]
derive mk signatures concrete tyname vars total ((field, _, fieldtyp), ctors) =
  do (sign, body) <-
       if length ctors == total
       then function deriveTotalLabel
       else function derivePartialLabel

     return $
       if signatures
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

    -- Build a single record label definition for labels that might fail.
    derivePartialLabel = (if concrete then mono else poly, body)
      where
        mono = forallT prettyVars (return []) [t| $(inputType) :~> $(return prettyFieldtyp) |]
        poly = forallT forallVars (return [])
          [t| (ArrowChoice $(arrow), ArrowFail String $(arrow))
              => Lens $(arrow) $(inputType) $(return prettyFieldtyp) |]
        body = [| lens (fromRight . $(getter)) (fromRight . $(setter)) |]
          where
            getter    = [| arr (\    p  -> $(caseE [|p|] (cases (bodyG [|p|]      ) ++ wild))) |]
            setter    = [| arr (\(v, p) -> $(caseE [|p|] (cases (bodyS [|p|] [|v|]) ++ wild))) |]
            cases b   = map (\ctor -> match (recP ctor []) (normalB b) []) ctors
            wild      = [match wildP (normalB failure) []]
            failure   = [| Left $(litE (stringL name) `sigE` [t| String |] ) |]
            bodyS p v = [| Right $( record p field v ) |]
            bodyG p   = [| Right $( varE field `appE` p ) |]

    -- Build a single record label definition for labels that cannot fail.
    deriveTotalLabel = (if concrete then mono else poly, body)
      where
        mono = forallT prettyVars (return []) [t| $(inputType) :-> $(return prettyFieldtyp) |]
        poly = forallT forallVars (return [])
          [t| Arrow $(arrow) => Lens $(arrow) $(inputType) $(return prettyFieldtyp) |]
        body = [| lens $(getter) $(setter) |]
          where
            getter = [| arr $(varE field) |]
            setter = [| arr (\(v, p) -> $(record [| p |] field [| v |])) |]

    -- Compute the type (including type variables of the record datatype.
    inputType = return $ foldr (flip AppT) (ConT tyname) (map tvToVarT (reverse prettyVars))

    -- Convert a type variable binder to a regular type variable.
    tvToVarT (PlainTV  tv     ) = VarT tv
    tvToVarT (KindedTV tv kind) = SigT (VarT tv) kind

    -- Prettify type variables.
    arrow          = varT (mkName "arr")
    prettyVars     = map prettyTyVar vars
    forallVars     = PlainTV (mkName "arr") : prettyVars
    prettyFieldtyp = prettyType fieldtyp

    -- Q style record updating.
    record rec fld val = val >>= \v -> recUpdE rec [return (fld, v)]

    -- Build a function declaration with both a type signature and body.
    function (s, b) = liftM2 (,) 
        (sigD label s)
        (funD label [ clause [] (normalB b) [] ])

fromRight :: (ArrowChoice a, ArrowFail e a) => a (Either e d) d
fromRight = failArrow ||| returnA

-------------------------------------------------------------------------------

-- Helper functions to prettify type variables.

prettyName :: Name -> Name
prettyName tv = mkName (takeWhile (/='_') (show tv))

prettyTyVar :: TyVarBndr -> TyVarBndr
prettyTyVar (PlainTV  tv   ) = PlainTV (prettyName tv)
prettyTyVar (KindedTV tv ki) = KindedTV (prettyName tv) ki

prettyType :: Type -> Type
prettyType (ForallT xs cx ty) = ForallT (map prettyTyVar xs) (map prettyPred cx) (prettyType ty)
prettyType (VarT nm         ) = VarT (prettyName nm)
prettyType (AppT ty tx      ) = AppT (prettyType ty) (prettyType tx)
prettyType (SigT ty ki      ) = SigT (prettyType ty) ki
prettyType ty                 = ty

prettyPred :: Pred -> Pred
prettyPred (ClassP nm tys) = ClassP (prettyName nm) (map prettyType tys)
prettyPred (EqualP ty tx ) = EqualP (prettyType ty) (prettyType tx)

-- IsString instances for TH types.

instance IsString Exp where
  fromString = VarE . mkName

instance IsString (Q Pat) where
  fromString = varP . mkName

instance IsString (Q Exp) where
  fromString = varE . mkName

