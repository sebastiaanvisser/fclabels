{- |
Template Haskell functions for automatically generating labels for algebraic
datatypes, newtypes and GADTs.
-}

{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , TemplateHaskell
  , TypeOperators
  , CPP #-}

module Data.Label.Derive
(

-- * Generate labels in scope.
  mkLabel
, mkLabels
, mkLabelsNamed
, mkLabelsWith

-- * Produce labels as expressions.
, getLabel
, getLabelWith

-- * Default naming function.
, defaultNaming
)
where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Char (toLower, toUpper)
import Data.Foldable (Foldable, toList)
import Data.Label.Point hiding (id)
import Data.List (groupBy, sortBy, delete, nub)
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
mkLabels = liftM concat . mapM (mkLabelsWith defaultNaming True False False True)

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
mkLabelsNamed mk = liftM concat . mapM (mkLabelsWith mk True False False True)

getLabel :: Name -> Q Exp
getLabel = getLabelWith False

getLabelWith
  :: Bool
  -> Name
  -> Q Exp
getLabelWith failing name =
  do info   <- reify name
     labels <- generateLabels id failing False info
     let bodies  =        map (\(LabelExpr _ _ _ b) -> b) labels
         types   =        map (\(LabelExpr _ _ t _) -> t) labels
         context = head $ map (\(LabelExpr _ c _ _) -> c) labels
         vars    = head $ map (\(LabelExpr v _ _ _) -> v) labels
     tupE bodies `sigE` forallT vars context (foldl appT (tupleT (length bodies)) types)

-- | Low level label derivation function.

mkLabelsWith
  :: (String -> String) -- ^ Supply a function to perform custom label naming.
  -> Bool               -- ^ Generate type signatures or not.
  -> Bool               -- ^ Generate concrete type or abstract type. When
                        --   true the signatures will be concrete and can only
                        --   be used in the appropriate context. Total lenses
                        --   will use (`:->`) and partial labels will use
                        --   either `Lens Partial` or `Lens Failing` dependent
                        --   on the following flag:
  -> Bool               -- ^ Use `ArrowFail` for failure instead of `ArrowZero`.
  -> Bool               -- ^ Generate inline pragma or not.
  -> Name               -- ^ The type to derive labels for.
  -> Q [Dec]

mkLabelsWith mk sigs concrete failing inl name =
  do info   <- reify name
     labels <- generateLabels mk failing concrete info
     decls  <- forM labels $ \l ->
       case l of
         LabelExpr {} -> return []
         LabelDecl n i v c t b ->
           do bdy <- pure <$> funD n [clause [] (normalB b) []]
              prg <- if inl then pure <$> i else return []
              typ <- if sigs
                       then pure <$> sigD n (forallT v c t)
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

data Label
 = LabelDecl
     Name              -- The label name.
     DecQ              -- An INLINE pragma for the label.
     [TyVarBndr]       -- The type variables requiring forall.
     CxtQ              -- The context.
     TypeQ             -- The type.
     ExpQ              -- The label body.
 | LabelExpr
     [TyVarBndr]       -- The type variables requiring forall.
     CxtQ              -- The context.
     TypeQ             -- The type.
     ExpQ              -- The label body.

data Field c = Field
  (Maybe Name)         -- Name of the field, when there is one.
  Bool                 -- Forced to be mono because of type shared with other fields.
  Type                 -- Type of the field.
  c                    -- Occurs in this/these constructors.
  deriving (Eq, Functor, Foldable)

data Context = Context
  Int                  -- Field index.
  Name                 -- Constructor name.
  Con                  -- Constructor.
  deriving Eq

data Typing = Typing
  TypeQ                -- The lens input type.
  TypeQ                -- The lens output type.
  [TyVarBndr]          -- All used type variables.
  TypeQ                -- The type constructor.

-- Generate the labels for all the record fields in the data type.

generateLabels :: (String -> String) -> Bool -> Bool -> Info -> Q [Label]
generateLabels mk failing concrete info =

 do -- Only process data and newtype declarations, filter out all
    -- constructors and the type variables.
    let (name, cons, vars) =
          case info of
            TyConI (DataD    _ n vs cs _) -> (n, cs,  vs)
            TyConI (NewtypeD _ n vs c  _) -> (n, [c], vs)
            _ -> fclError "Can only derive labels for datatypes and newtypes."

        -- We are only interested in lenses of record constructors.
        fields = groupFields mk cons

    forM fields $ generateLabel failing concrete name vars (length cons)

groupFields :: (String -> String) -> [Con] -> [Field [Context]]
groupFields mk
  = map (rename mk)
  . concatMap (\fs -> let cons = concat (toList <$> fs) in nub $ map (fmap (const cons)) fs)
  . groupBy eq
  . sortBy (comparing name)
  . concatMap constructorFields
  where name (Field n _ _ _) = n
        eq f g = False `fromMaybe` ((==) <$> name f <*> name g)
        rename f (Field n a b c) =
          Field (mkName . f . nameBase <$> n) a b c

constructorFields :: Con -> [Field Context]
constructorFields con =

  case con of

    NormalC c fs -> one <$> zip [0..] fs
      where one (i, f@(_, ty)) = Field Nothing mono ty (Context i c con)
              where fsTys = map (typeVariables . snd) (delete f fs)
                    mono  = any (\x -> any (elem x) fsTys) (typeVariables ty)

    RecC c fs -> one <$> zip [0..] fs
      where one (i, f@(n, _, ty)) = Field (Just n) mono ty (Context i c con)
              where fsTys = map (typeVariables . trd) (delete f fs)
                    mono  = any (\x -> any (elem x) fsTys) (typeVariables ty)
                    trd (_, _, x) = x

    InfixC a c b -> one <$> [(0, a), (1, b)]
      where one (i, (_, ty)) = Field Nothing mono ty (Context i c con)
              where fsTys = map (typeVariables . snd) [a, b]
                    mono  = any (\x -> any (elem x) fsTys) (typeVariables ty)

    ForallC _ _ c -> constructorFields c

generateLabel
  :: Bool
  -> Bool
  -> Name
  -> [TyVarBndr]
  -> Int
  -> Field [Context]
  -> Q Label

generateLabel failing concrete tyname tyvars conCount
              field@(Field name forcedMono fieldtyp ctors) =

  do let total = length ctors == conCount
         mono  = forcedMono || isMonomorphic fieldtyp tyvars

     (Typing tyI tyO vars typ) <- computeTypes mono fieldtyp tyname tyvars

     let cat     = varT (mkName "cat")
         tvs     = if concrete
                   then vars
                   else PlainTV (mkName "cat") : vars
         forall  = forallT tvs (return [])
         failE   = if failing
                   then [| failArrow |]
                   else [| zeroArrow |]
         getT    = [| arr $(getter total field) |]
         putT    = [| arr $(setter total field) |]
         getP    = [| $(failE) ||| id <<< $getT |]
         putP    = [| $(failE) ||| id <<< $putT |]
         failP   = if failing
                   then classP ''ArrowFail [ [t| String |], cat]
                   else classP ''ArrowZero [cat]
         ctx     = if total
                   then cxt [ classP ''ArrowApply  [cat] ]
                   else cxt [ classP ''ArrowChoice [cat]
                            , classP ''ArrowApply  [cat]
                            , failP
                            ]
         body    = if total
                   then [| Poly.point $ Point $getT (modifier $getT $putT) |]
                   else [| Poly.point $ Point $getP (modifier $getP $putP) |]
         cont    = if concrete
                   then cxt []
                   else ctx
         partial = if failing
                   then [t| Failing String |]
                   else [t| Partial |]
         concTy  = if total
                   then [t| $typ Total $tyI $tyO |]
                   else [t| $typ $partial $tyI $tyO |]
         ty      = if concrete
                   then concTy
                   else [t| $typ $cat $tyI $tyO |]

     return $
       case name of
         Nothing -> LabelExpr tvs cont ty body
         Just n  ->

#if MIN_VERSION_template_haskell(2,8,0)
           -- Generate an inline declaration for the label.
           -- Type of InlineSpec removed in TH-2.8.0 (GHC 7.6)
           let inline = PragmaD (InlineP n Inline FunLike (FromPhase 0))
#else
           let inline = PragmaD (InlineP n (InlineSpec True True (Just (True, 0))))
#endif
            in LabelDecl n (return inline) tvs cont ty body

-- Build a total polymorphic modification function from a getter and setter.

modifier :: ArrowApply cat => cat f o -> cat (i, f) g -> cat (cat o i, f) g
modifier g m = m . first app . arr (\(n, (f, o)) -> ((n, o), f)) . second (id &&& g)
{-# INLINE modifier #-}

-------------------------------------------------------------------------------

getter :: Bool -> Field [Context] -> Q Exp
getter total (Field mn _ _ cons) =
  do let pt = mkName "f"
         nm = maybe (tupE []) (litE . StringL . nameBase) mn
         wild = if total then [] else [match wildP (normalB [| Left $(nm) |]) []]
         rght = if total then id else appE [| Right |]
         mkCase (Context i _ c) = match pat (normalB (rght var)) []
           where (pat, var) = case1 i c
     lamE [varP pt]
          (caseE (varE pt) (map mkCase cons ++ wild))
  where
  case1 i con =
    case con of
      NormalC c fs  -> let s = take (length fs) in (conP c (s pats), var)
      RecC    c fs  -> let s = take (length fs) in (conP c (s pats), var)
      InfixC  _ c _ -> (infixP (pats !! 0) c (pats !! 1), var)
      ForallC _ _ c -> case1 i c
    where fresh = mkName . pure <$> delete 'f' ['a'..'z']
          pats1 = varP <$> fresh
          pats  = replicate i wildP ++ [pats1 !! i] ++ repeat wildP
          var   = varE (fresh !! i)

setter :: Bool -> Field [Context] -> Q Exp
setter total (Field mn _ _ cons) =
  do let pt = mkName "f"
         md = mkName "v"
         nm = maybe (tupE []) (litE . StringL . nameBase) mn
         wild = if total then [] else [match wildP (normalB [| Left $(nm) |]) []]
         rght = if total then id else appE [| Right |]
         mkCase (Context i _ c) = match pat (normalB (rght var)) []
           where (pat, var) = case1 i c
     lamE [tupP [varP md, varP pt]]
          (caseE (varE pt) (map mkCase cons ++ wild))
  where
  case1 i con =
    case con of
      NormalC c fs  -> let s = take (length fs) in (conP c (s pats), apps (conE c) (s vars))
      RecC    c fs  -> let s = take (length fs) in (conP c (s pats), apps (conE c) (s vars))
      InfixC  _ c _ -> ( infixP (pats !! 0) c (pats !! 1)
                       , infixE (Just (vars !! 0)) (conE c) (Just (vars !! 1))
                       )
      ForallC _ _ c -> case1 i c
    where fresh     = mkName . pure <$> delete 'f' (delete 'v' ['a'..'z'])
          pats1     = varP <$> fresh
          pats      = take i pats1 ++ [wildP] ++ drop (i + 1) pats1
          vars1     = varE <$> fresh
          v         = varE (mkName "v")
          vars      = take i vars1 ++ [v] ++ drop (i + 1) vars1
          apps f as = foldl appE f as

-------------------------------------------------------------------------------

isMonomorphic :: Type -> [TyVarBndr] -> Bool
isMonomorphic field vars =

  let fieldVars = typeVariables field
      varNames  = fromTyVarBndr <$> vars
      usedVars  = filter (`elem` fieldVars) varNames
   in null usedVars

computeTypes :: Bool -> Type -> Name -> [TyVarBndr] -> Q Typing
computeTypes mono field datatype vars =

  do let fieldVars = typeVariables field
         tyO       = return field
         varNames  = fromTyVarBndr <$> vars
         usedVars  = filter (`elem` fieldVars) varNames
         tyI       = foldr (flip appT) (conT datatype)
                           (return . tvToVarT <$> reverse vars)

     if mono
       then return $ Typing
               (prettyType <$> tyI)
               (prettyType <$> tyO)
               (mapTyVarBndr pretty <$> vars)
               [t| Mono.Lens |]
       else
         do let names = return <$> ['a'..'z']
                used  = show . pretty <$> varNames
                free  = filter (not . (`elem` used)) names
            subs <- forM (zip usedVars free) (\(a, b) -> (,) a <$> newName b)
            let rename = mapTypeVariables (\a -> a `fromMaybe` lookup a subs)

            return $ Typing
              (prettyType <$> [t| $tyI -> $(rename <$> tyI) |])
              (prettyType <$> [t| $tyO -> $(rename <$> tyO) |])
              (mapTyVarBndr pretty <$> vars ++ (PlainTV . snd <$> subs))
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

