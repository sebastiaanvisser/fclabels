{- |
Template Haskell functions for automatically generating labels for algebraic
datatypes, newtypes and GADTs.
-}

{-# LANGUAGE
    TemplateHaskell
  , TypeOperators
  , CPP #-}

module Data.Label.Derive
( mkLabel
, mkLabels
, mkLabelsNamed
, mkLabelsWith
, defaultNaming
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
import Prelude hiding ((.), id)

import qualified Data.Label.Mono as Mono
import qualified Data.Label.Poly as Poly

-- | Derive lenses including type signatures for all the record selectors for a
-- collection of datatypes. The types will be polymorphic and can be used in an
-- arbitrary context.

mkLabels :: [Name] -> Q [Dec]
mkLabels = liftM concat . mapM (mkLabelsWith defaultNaming True False True)

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

mkLabelsNamed :: (String -> String) -> [Name] -> Q [Dec]
mkLabelsNamed mk = liftM concat . mapM (mkLabelsWith mk True False True)

-- | Low level label derivation function.

mkLabelsWith
  :: (String -> String) -- ^ Supply a function to perform custom label naming.
  -> Bool               -- ^ Generate type signatures or not.
  -> Bool               -- ^ Generate concrete type or abstract type. When
                        -- true the signatures will be concrete and can only be
                        -- used in the appropriate context. Total lenses will
                        -- use (`:->`) and partial labels will use (`:~>`).
  -> Bool               -- ^ Generate inline pragma or not.
  -> Name               -- ^ The type to derive labels for.
  -> Q [Dec]

mkLabelsWith mk sigs conc inl name =
  do info   <- reify name
     labels <- generateLabels mk info
     decls  <- forM labels $ \(Label n c a b i) ->
       do bdy <- pure <$> funD n [clause [] (normalB b) []]
          prg <- if inl then pure <$> i else return []
          typ <- if sigs
                   then pure <$> sigD n (if conc then c else a)
                   else return []
          return (concat [prg, typ, bdy])
     return (concat decls)

-- | Generate a name for the label. If the original selector starts with an
-- underscore, remove it and make the next character lowercase. Otherwise, add
-- 'l', and make the next character uppercase.

defaultNaming :: String -> String
defaultNaming field =
  case field of
    '_' : c : rest -> toLower c : rest
    f : rest       -> 'l' : toUpper f : rest
    n              -> fclError ("Cannot derive label for record selector with name: " ++ n)

-------------------------------------------------------------------------------

data Label = Label
  Name   -- The label name.
  TypeQ  -- The concreate type signature.
  TypeQ  -- The abstract type signature.
  ExpQ   -- The label body.
  DecQ   -- An INLINE pragma for the label.

data LensType = LensType
  TypeQ        -- The lens input type.
  TypeQ        -- The lens output type.
  [TyVarBndr]  -- All used type variables.
  TypeQ        -- The type constructor.

-- Generate the labels for all the record fields in the data type.

generateLabels :: (String -> String) -> Info -> Q [Label]
generateLabels mk info =

 do -- Only process data and newtype declarations, filter out all
    -- constructors and the type variables.
    let (tyname, cons, tyvars) =
          case info of
            TyConI (DataD    _ n vs cs _) -> (n, cs,  vs)
            TyConI (NewtypeD _ n vs c  _) -> (n, [c], vs)
            _ -> fclError "Can only derive labels for datatypes and newtypes."

        -- We are only interested in lenses of record constructors.
        recordOnly = groupByCtor [ (f, n) | RecC n fs <- cons, f <- fs ]

    forM recordOnly $ \((field, _, ty), ctors) ->
      do generateLabel mk tyname tyvars (length cons) field ty ctors

    where groupByCtor
            = map (\xs -> (fst (head xs), map snd xs))
            . groupBy ((==) `on` (fst3 . fst))
            . sortBy (comparing (fst3 . fst))
            where fst3 (a, _, _) = a

-- Generate the code for a single label.

generateLabel
  :: (String -> String)
  -> Name
  -> [TyVarBndr]
  -> Int
  -> Name
  -> Type
  -> [Name]
  -> Q Label

generateLabel mk tyname tyvars total field fieldtyp ctors =

     -- Compute the type (including type variables of the record datatype.
  do (LensType inTy outTy vars typ) <- computeTypes fieldtyp tyname tyvars

     let isTotal = length ctors == total

         cat    = varT (mkName "cat")
         catTv  = PlainTV (mkName "cat")
         forall = forallT (catTv : vars) (return [])

         -- Q style record updating.
         record rec fld val = val >>= \v -> recUpdE rec [return (fld, v)]

         -- The label name.
         name = mkName (mk (nameBase field))

#if MIN_VERSION_template_haskell(2,8,0)
         -- Generate an inline declaration for the label.
         -- Type of InlineSpec removed in TH-2.8.0 (GHC 7.6)
         inline = return $ PragmaD (InlineP name Inline FunLike (FromPhase 0))
#else
         inline = Preturn $ ragmaD (InlineP name (InlineSpec True True (Just (True, 0))))
#endif

         -- The total getter and setter arrows. Directly used by total lenses,
         -- indirectly used by partial lenses.
         totalG = [| arr $(varE field) |]
         totalM = [| modifier $(varE field) (\(v, f) -> $(record [| f |] field [| v |])) |]

         ---------------------------------------------------------------------------
         -- Build a single record label definition for labels that might fail.

         partialPoint = (conc, abst, body)
           where
             conc = forall [t| $(inTy) :~> $(outTy) |]
             abst = forall [t| (ArrowChoice $(cat), ArrowFail String $(cat), ArrowApply $(cat))
                        => $(typ) $(cat) $(inTy) $(outTy) |]
             body = [| Poly.Lens
                         $ Point (failArrow ||| $(totalG) <<< $(caseG))
                                 (failArrow ||| $(totalM) <<< $(caseM))
                     |]
             bodyG f = [| Right $(f) |]
             cases b = map (\c -> match (recP c []) (normalB b) []) ctors
             wild    = [match wildP (normalB failure) []]
             failure = [| Left $(litE (stringL (nameBase name)) `sigE` [t| String |] ) |]
             caseG   = [| arr (\f      -> $(caseE [|f|] (cases (bodyG [|f     |]) ++ wild))) |]
             caseM   = [| arr (\(m, f) -> $(caseE [|f|] (cases (bodyG [|(m, f)|]) ++ wild))) |]

         ---------------------------------------------------------------------------
         -- Build a single record label definition for labels that cannot fail.

         totalPoint = (conc, abst, body)
           where
             conc = forall [t| $(inTy) :-> $(outTy) |]
             abst = forall [t| ArrowApply $(cat) => $(typ) $(cat) $(inTy) $(outTy) |]
             body = [| Poly.Lens (Point $(totalG) $(totalM)) |]

     let (conc, abst, body) =
           if isTotal
           then totalPoint
           else partialPoint

     return (Label name conc abst body inline)

-- Build a total polymorphic modification function from a pure getter and setter.

modifier :: ArrowApply cat => (f -> o) -> ((i, f) -> g) -> cat (cat o i, f) g
modifier pg pm
  = arr pm
  . first app
  . arr (\(m, (f, o)) -> ((m, o), f))
  . second (id &&& arr pg)
{-# INLINE modifier #-}

-------------------------------------------------------------------------------

computeTypes :: Type -> Name -> [TyVarBndr] -> Q LensType
computeTypes field datatype vars =

  do let fieldVars = typeVariables field
         outTy     = return field
         varNames  = fromTyVarBndr <$> vars
         usedVars  = filter (`elem` fieldVars) varNames
         inTy      = foldr (flip appT) (conT datatype)
                           (return . tvToVarT <$> reverse vars)

     case usedVars of
       [] -> return $ LensType
         (prettyType <$> inTy)
         (prettyType <$> outTy)
         (mapTyVarBndr pretty <$> vars)
         [t| Mono.Lens |]
       vs ->
         do let names = return <$> ['a'..'z']
                used  = show . pretty <$> varNames
                free  = filter (not . (`elem` used)) names
            subs <- forM (zip vs free) (\(a, b) -> (,) a <$> newName b)
            let rename = mapTypeVariables (\a -> a `fromMaybe` lookup a subs)

            return $ LensType
              (prettyType <$> [t| $(inTy)  -> $(rename <$> inTy)  |])
              (prettyType <$> [t| $(outTy) -> $(rename <$> outTy) |])
              (mapTyVarBndr pretty <$> (vars ++ (PlainTV . snd <$> subs)))
              [t| Poly.Lens |]

  where -- Convert a binder to a regular type variable.
        tvToVarT (PlainTV  tv     ) = VarT tv
        tvToVarT (KindedTV tv kind) = SigT (VarT tv) kind

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

-- Throw a fclabels specific error.

fclError :: String -> a
fclError err = error ("Data.Label.Derive: " ++ err)

