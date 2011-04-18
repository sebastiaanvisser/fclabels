{-# LANGUAGE TemplateHaskell #-}
module Data.Label.Derive
( mkLabels
, mkLabelsNoTypes
) where

import Control.Arrow
import Control.Category
import Control.Monad
import Data.Char
import Data.Function (on)
import Data.Label.Abstract
import Data.List
import Data.Ord
import Language.Haskell.TH.Syntax
import Prelude hiding ((.), id)

-- | Derive lenses including type signatures for all the record selectors in a
-- datatype.

mkLabels :: [Name] -> Q [Dec]
mkLabels = liftM concat . mapM (mkOneLabel True)

-- | Derive lenses without type signatures for all the record selectors in a
-- datatype.

mkLabelsNoTypes :: [Name] -> Q [Dec]
mkLabelsNoTypes = liftM concat . mapM (mkOneLabel False)

-- Helpers to generate all labels.

mkOneLabel :: Bool -> Name -> Q [Dec]
mkOneLabel sigs name =
 do i <- reify name
    let -- Only process data and newtype declarations, filter out all
        -- constructors and the type variables.
        (tyname, cons, vars) =
          case i of
            TyConI (DataD    _ n vs cs _) -> (n, cs,  vs)
            TyConI (NewtypeD _ n vs c  _) -> (n, [c], vs)
            _                             -> fclError "Can only derive mkOneLabel for datatypes and newtypes."

        -- We are only interested in lenses of record constructors.
        recordOnly = groupByCtor [ (f, n) | RecC n fs <- cons, f <- fs ]

    concat `liftM` mapM (codeGen sigs tyname vars (length cons)) recordOnly

    where groupByCtor = map (\xs -> (fst (head xs), map snd xs))
                      . groupBy ((==) `on` (fst3 . fst))
                      . sortBy (comparing (fst3 . fst))
          fst3 (a, _, _) = a

codeGen :: Bool -> Name -> [TyVarBndr] -> Int -> (VarStrictType, [Name]) -> Q [Dec]
codeGen sigs tyname vars total ((field, _, fieldtyp), ctors) =
   if total == length ctors
     then label1
     else label

  where

    -- Generate a name for the lens. If the original selector starts with an _,
    -- remove it and make the next character lowercase. Otherwise, add 'l', and
    -- make the next character uppercase.
    name = mkName $
            case nameBase field of
              '_' : c : rest -> toLower c : rest
              f : rest       -> 'l' : toUpper f : rest
              _              -> fclError "Invalid name"

    cases b = map (\ctor -> Match (RecP ctor []) (NormalB b) [] ) ctors

    toyalTyName = foldr (flip AppT) (ConT tyname) (map tvToVarT (reverse vars))

    tvToVarT (PlainTV v) = VarT v
    tvToVarT _           = fclError "No support for special-kinded type variables."

    getterCases = cases getterBody ++ wild
    setterCases = cases setterBody ++ wild

    typ = 
      do t <- [t| (ArrowChoice (~>), ArrowZero (~>)) => Lens (~>) $( return toyalTyName ) $( return fieldtyp ) |]
         return (ForallT vars [] t)

    wild = [Match (WildP ) (NormalB (ce "Left" `AppE` ce "()")) []]

    setterBody = ce "Right" `AppE` RecUpdE (ve "p") [(mkName (nameBase field), ve "v")]
    getterBody = ce "Right" `AppE` (ve (nameBase field) `AppE` ve "p")

    getter = ve "arr" `AppE` ([vp "p"               ] `LamE` CaseE (ve "p") getterCases)
    setter = ve "arr" `AppE` ([TupP [vp "v", vp "p"]] `LamE` CaseE (ve "p") setterCases)

    ce x = ConE (mkName x)
    ve x = VarE (mkName x)
    vp x = VarP (mkName x)

    body = [| let c = zeroArrow ||| returnA
              in lens (c . $(return getter))
                      (c . $(return setter))
            |]

    label = 
      do t <- typ
         b <- body
         return
           [ SigD name t
           , FunD name [ Clause [] (NormalB b) [] ]
           ]

    -------------------------------------------------

    label1 = 
      do t1 <- typ1
         b1 <- body1
         return
           [ SigD name t1
           , FunD name [ Clause [] (NormalB b1) [] ]
           ]

    typ1 = 
      do t <- [t| Arrow (~>) => Lens (~>) $( return toyalTyName ) $( return fieldtyp ) |]
         return (ForallT vars [] t)

    body1 = [| lens $(return getter1)
                    $(return setter1)
             |]

    getter1 = ve "arr" `AppE` getterBody1
    setter1 = ve "arr" `AppE` ([TupP [vp "v", vp "p"]] `LamE` setterBody1)

    setterBody1 = RecUpdE (ve "p") [(mkName (nameBase field), ve "v")]
    getterBody1 = ve (nameBase field)

fclError :: String -> a
fclError err = error ("Data.Label.Derive: " ++ err)

