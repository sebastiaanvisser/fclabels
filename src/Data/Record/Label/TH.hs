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
    let -- only process data and newtype declarations
        cs' = case i of
                TyConI (DataD _ _ _ cs _)   -> cs
                TyConI (NewtypeD _ _ _ c _) -> [c]
                _ -> []
        -- we're only interested in labels of record constructors
        ls' = [ l | RecC _ ls <- cs', l <- ls ]
    return (map mkLabel1 ls')

mkLabel1 :: VarStrictType -> Dec
mkLabel1 (name, _, _) =
    -- Generate a name for the label:
    -- If the original selector starts with an _, remove it and make the next
    -- character lowercase.  Otherwise, add 'l', and make the next character
    -- uppercase.
    let n = mkName $ case nameBase name of
                ('_' : c : rest) -> toLower c : rest
                (f : rest)       -> 'l' : toUpper f : rest
                _                -> ""
    in FunD n [Clause [] (NormalB (
           AppE (AppE (VarE (mkName "label")) (VarE name)) -- getter
                (LamE [VarP (mkName "b"), VarP (mkName "a")] -- setter
                      (RecUpdE (VarE (mkName "a")) [(name, VarE (mkName "b"))]))
                                   )) []]

