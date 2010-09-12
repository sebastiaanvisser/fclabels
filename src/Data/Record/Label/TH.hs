module Data.Record.Label.TH
( mkLabels
, mkLabelsNoTypes
) where

import Control.Monad
import Data.Char
import Language.Haskell.TH.Syntax

-- | Derive lenses including type signatures for all the record selectors in a
-- datatype.

mkLabels :: [Name] -> Q [Dec]
mkLabels = liftM concat . mapM (labels True)

-- | Derive lenses without type signatures for all the record selectors in a
-- datatype.

mkLabelsNoTypes :: [Name] -> Q [Dec]
mkLabelsNoTypes = liftM concat . mapM (labels False)

-- Helpers to generate all labels.

labels :: Bool -> Name -> Q [Dec]
labels sigs n =
 do i <- reify n
    let -- Only process data and newtype declarations, filter out all
        -- constructors and the type variables.
        (cs',vars) =
          case i of
            TyConI (DataD    _ _ vs cs _) -> (cs , vs)
            TyConI (NewtypeD _ _ vs c  _) -> ([c], vs)
            _                             -> ([], undefined)

        -- We are only interested in lenses of record constructors.
        ls' = [ l | RecC _ ls <- cs', l <- ls ]

    return (concatMap (label sigs n vars) ls')

-- Helpers to generate a single labels.

label :: Bool -> Name -> [TyVarBndr] -> VarStrictType -> [Dec]
label withType typeName binders (field, _, typ) =
  if withType
    then [signature, body]
    else [body]

  where
    appTv w (PlainTV n) = AppT w (VarT n)
    appTv _ v           = error ("Kinded type variable not supported: " ++ show v)

    -- Generate a name for the lens. If the original selector starts with an _,
    -- remove it and make the next character lowercase. Otherwise, add 'l', and
    -- make the next character uppercase.
    name = mkName $
            case nameBase field of
              '_' : c : rest -> toLower c : rest
              f : rest       -> 'l' : toUpper f : rest
              _              -> error "Invalid name"

    -- The source type of a lens.
    source = foldl appTv (ConT typeName) binders

    -- Construct the lens type.
    signature = SigD name (ForallT binders [] (ConT (mkName ":->") `AppT` source `AppT` typ))

    -- Construct the lens body.
    body = 
      let getter = VarE field 
          setter = [VarP (mkName "b"), VarP (mkName "a")]
                     `LamE` RecUpdE (VarE (mkName "a")) [(field, VarE (mkName "b"))]
          lens   = VarE (mkName "lens") `AppE` getter `AppE` setter
      in FunD name [ Clause [] (NormalB lens) [] ]

