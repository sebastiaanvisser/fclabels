module Data.Record.Label.TH (mkLabels, mkLabelsNoTypes) where

import Control.Monad
import Data.Char
import Language.Haskell.TH.Syntax

-- | Derive lenses including type signatures for all the record selectors in a datatype.
mkLabels :: [Name] -> Q [Dec]
mkLabels = liftM concat . mapM (mkLabels1 True)

-- | Derive lenses without type signatures for all the record selectors in a datatype.
mkLabelsNoTypes :: [Name] -> Q [Dec]
mkLabelsNoTypes = liftM concat . mapM (mkLabels1 False)

-- Helpers.

mkLabels1 :: Bool -> Name -> Q [Dec]
mkLabels1 sigs n = do
    i <- reify n
    let -- only process data and newtype declarations, filter out all constructors and the type variables
        (cs',vars) = case i of
                TyConI (DataD    _ _ vs cs _) -> (cs , vs)
                TyConI (NewtypeD _ _ vs c  _) -> ([c], vs)
                _ -> ([], undefined)
        -- we are only interested in lenses of record constructors
        ls' = [ l | RecC _ ls <- cs', l <- ls ]
    return (concatMap (mkLabel1 sigs n vars) ls')

mkLabel1 :: Bool -> Name -> [TyVarBndr] -> VarStrictType -> [Dec]
mkLabel1 sigs typeName binders (name, _, t) =
    let -- Generate a name for the lens:
        -- If the original selector starts with an _, remove it and make the next
        -- character lowercase.  Otherwise, add 'l', and make the next character
        -- uppercase.
        lensName = mkName $ case nameBase name of
                ('_' : c : rest) -> toLower c : rest
                (f : rest)       -> 'l' : toUpper f : rest
                _                -> error "Invalid name"

        -- The source type of a lens
        source    = foldl appTv (ConT typeName) binders

        -- The type of the lens
        lensType = ForallT binders [] $ AppT (AppT (ConT $ mkName ":->") source) t

        in (if sigs then [SigD lensName lensType] else []) ++ [functionBody lensName name]

appTv :: Type -> TyVarBndr -> Type
appTv t (PlainTV n) = AppT t (VarT n)
appTv _ v           = error $ "Kinded type variable not supported: " ++ show v


functionBody :: Name -> Name -> Dec
functionBody lensName fieldName = 
  FunD lensName      [
    Clause [] (
        NormalB  (
          AppE (AppE (VarE (mkName "lens")) (VarE fieldName)) -- getter
               (LamE [VarP (mkName "b"), VarP (mkName "a")]    -- setter
                     (RecUpdE (VarE (mkName "a")) [(fieldName, VarE (mkName "b"))])
               )
                 )
              ) []    ]
