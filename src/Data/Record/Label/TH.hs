module Data.Record.Label.TH (mkLabels) where

import Control.Monad
import Data.Char
import Language.Haskell.TH.Syntax

-- | Derive labels for all the record selector in a datatype.
mkLabels :: [Name] -> Q [Dec]
mkLabels = liftM concat . mapM mkLabels1

mkLabels1 :: Name -> Q [Dec]
mkLabels1 n = do
    i <- reify n
    let -- only process data and newtype declarations, filter out all constructors and the type variables
        (cs',vars) = case i of
                TyConI (DataD    _ _ vs cs _) -> (cs , vs)
                TyConI (NewtypeD _ _ vs c  _) -> ([c], vs)
                _ -> ([], undefined)
        -- we are only interested in labels of record constructors
        ls' = [ l | RecC _ ls <- cs', l <- ls ]
    return (concatMap (mkLabel1 n vars) ls')

mkLabel1 :: Name -> [TyVarBndr] -> VarStrictType -> [Dec]
mkLabel1 typeName binders (name, _, t) =
    let -- Generate a name for the label:
        -- If the original selector starts with an _, remove it and make the next
        -- character lowercase.  Otherwise, add 'l', and make the next character
        -- uppercase.
        labelName = mkName $ case nameBase name of
                ('_' : c : rest) -> toLower c : rest
                (f : rest)       -> 'l' : toUpper f : rest
                _                -> error "Invalid name"

        -- The source type of a label
        source    = foldl appTv (ConT typeName) binders

        -- The type of the label
        labelType = ForallT binders [] $ AppT (AppT (ConT $ mkName ":->") source) t

        in [SigD labelName labelType, functionBody labelName name]

appTv :: Type -> TyVarBndr -> Type
appTv t (PlainTV n) = AppT t (VarT n)
appTv _ v           = error $ "Kinded type variable not supported: " ++ show v


functionBody :: Name -> Name -> Dec
functionBody labelName fieldName = 
  FunD labelName      [
    Clause [] (
        NormalB  (
          AppE (AppE (VarE (mkName "label")) (VarE fieldName)) -- getter
               (LamE [VarP (mkName "b"), VarP (mkName "a")]    -- setter
                     (RecUpdE (VarE (mkName "a")) [(fieldName, VarE (mkName "b"))])
               )
                 )
              ) []    ]
