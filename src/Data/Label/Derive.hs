{- |
Template Haskell functions for automatically generating labels for algebraic
datatypes, newtypes and GADTs. There are two basic modes of label generation,
the `mkLabels` family of functions create labels (and optionally type
signatures) in scope as top level funtions, the `getLabel` family of funtions
create labels as expressions that can be named and typed manually.

In the case of multi-constructor datatypes some fields might not always be
available and the derived labels will be partial. Partial labels are provided
with an additional type context that forces them to be only usable in the
`Partial' or `Failing` context.
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

-- * Produce labels as expressions.
, getLabel

-- * First class record labels.
, fclabels

-- * Low level derivation functions.
, mkLabelsWith
, getLabelWith
, defaultNaming
)
where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Char (toLower, toUpper)
import Data.Foldable (Foldable, toList)
import Data.Label.Point
import Data.List (groupBy, sortBy, delete, nub)
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.String
import Language.Haskell.TH
import Prelude hiding ((.), id)

import qualified Data.Label.Mono as Mono
import qualified Data.Label.Poly as Poly

-------------------------------------------------------------------------------
-- Publicly exposed functions.

-- | Derive labels including type signatures for all the record selectors for a
-- collection of datatypes. The types will be polymorphic and can be used in an
-- arbitrary context.

mkLabels :: [Name] -> Q [Dec]
mkLabels = liftM concat . mapM (mkLabelsWith defaultNaming True False False True)

-- | Derive labels including type signatures for all the record selectors in a
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

-- | Derive unnamed labels as n-tuples that can be named manually. The types
-- will be polymorphic and can be used in an arbitrary context.
--
-- Example:
--
-- > (left, right) = $(getLabel ''Either)
--
-- The lenses can now also be typed manually:
--
-- > left  :: (Either a b -> Either c b) :~> (a -> c)
-- > right :: (Either a b -> Either a c) :~> (b -> c)
--
-- Note: Because of the abstract nature of the generated lenses and the top
-- level pattern match, it might be required to use 'NoMonomorphismRestriction'
-- in some cases.

getLabel :: Name -> Q Exp
getLabel = getLabelWith True False False

-- | Low level label as expression derivation function.

getLabelWith
  :: Bool  -- ^ Generate type signatures or not.
  -> Bool  -- ^ Generate concrete type or abstract type. When true the
           --   signatures will be concrete and can only be used in the
           --   appropriate context. Total labels will use (`:->`) and partial
           --   labels will use either `Lens Partial` or `Lens Failing`
           --   dependent on the following flag:
  -> Bool  -- ^ Use `ArrowFail` for failure instead of `ArrowZero`.
  -> Name  -- ^ The type to derive labels for.
  -> Q Exp

getLabelWith sigs concrete failing name =
  do dec    <- reifyDec name
     labels <- generateLabels id concrete failing dec
     let bodies  =        map (\(LabelExpr _ _ _ b) -> b) labels
         types   =        map (\(LabelExpr _ _ t _) -> t) labels
         context = head $ map (\(LabelExpr _ c _ _) -> c) labels
         vars    = head $ map (\(LabelExpr v _ _ _) -> v) labels
     if sigs
       then tupE bodies `sigE`
              forallT vars context (foldl appT (tupleT (length bodies)) types)
       else tupE bodies

-- | Low level standalone label derivation function.

mkLabelsWith
  :: (String -> String) -- ^ Supply a function to perform custom label naming.
  -> Bool               -- ^ Generate type signatures or not.
  -> Bool               -- ^ Generate concrete type or abstract type. When
                        --   true the signatures will be concrete and can only
                        --   be used in the appropriate context. Total labels
                        --   will use (`:->`) and partial labels will use
                        --   either `Lens Partial` or `Lens Failing` dependent
                        --   on the following flag:
  -> Bool               -- ^ Use `ArrowFail` for failure instead of `ArrowZero`.
  -> Bool               -- ^ Generate inline pragma or not.
  -> Name               -- ^ The type to derive labels for.
  -> Q [Dec]

mkLabelsWith mk sigs concrete failing inl name =
  do dec <- reifyDec name
     mkLabelsWithForDec mk sigs concrete failing inl dec

-- | Default way of generating a label name from the Haskell record selector
-- name. If the original selector starts with an underscore, remove it and make
-- the next character lowercase. Otherwise, add 'l', and make the next
-- character uppercase.

defaultNaming :: String -> String
defaultNaming field =
  case field of
    '_' : c : rest -> toLower c : rest
    f : rest       -> 'l' : toUpper f : rest
    n              -> fclError ("Cannot derive label for record selector with name: " ++ n)

-- | Derive labels for all the record types in the supplied declaration. The
-- record fields don't need an underscore prefix. Multiple data types /
-- newtypes are allowed at once.
--
-- The advantage of this approach is that you don't need to explicitly hide the
-- original record accessors from being exported and they won't show up in the
-- derived `Show` instance.
--
-- Example:
--
-- > fclabels [d|
-- >   data Record = Record
-- >     { int  :: Int
-- >     , bool :: Bool
-- >     } deriving Show
-- >   |]
--
-- > ghci> modify int (+2) (Record 1 False)
-- > Record 3 False

fclabels :: Q [Dec] -> Q [Dec]
fclabels decls =
  do ds <- decls
     ls <- forM (ds >>= labels) (mkLabelsWithForDec id True False False False)
     return (concat ((delabelize <$> ds) : ls))
  where

  labels :: Dec -> [Dec]
  labels dec =
    case dec of
      DataD    {} -> [dec]
      NewtypeD {} -> [dec]
      _           -> []

  delabelize :: Dec -> Dec
  delabelize dec =
    case dec of
      DataD    ctx nm vars cs ns -> DataD    ctx nm vars (con <$> cs) ns
      NewtypeD ctx nm vars c  ns -> NewtypeD ctx nm vars (con c)      ns
      rest                       -> rest
    where con (RecC n vst) = NormalC n (map (\(_, s, t) -> (s, t)) vst)
          con c            = c

-------------------------------------------------------------------------------
-- Intermediate data types.

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

type Subst = [(Type, Type)]

data Context = Context
  Int                  -- Field index.
  Name                 -- Constructor name.
  Con                  -- Constructor.
  deriving (Eq, Show)

data Typing = Typing
  Bool                 -- Monomorphic type or polymorphic.
  TypeQ                -- The lens input type.
  TypeQ                -- The lens output type.
  [TyVarBndr]          -- All used type variables.

-------------------------------------------------------------------------------

mkLabelsWithForDec :: (String -> String) -> Bool -> Bool -> Bool -> Bool -> Dec -> Q [Dec]
mkLabelsWithForDec mk sigs concrete failing inl dec =
  do labels <- generateLabels mk concrete failing dec
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

-- Generate the labels for all the record fields in the data type.

generateLabels :: (String -> String) -> Bool -> Bool -> Dec -> Q [Label]
generateLabels mk concrete failing dec =

 do -- Only process data and newtype declarations, filter out all
    -- constructors and the type variables.
    let (name, cons, vars) =
          case dec of
            DataD    _ n vs cs _ -> (n, cs,  vs)
            NewtypeD _ n vs c  _ -> (n, [c], vs)
            _ -> fclError "Can only derive labels for datatypes and newtypes."

        -- We are only interested in lenses of record constructors.
        fields = groupFields mk cons

    forM fields $ generateLabel failing concrete name vars cons

groupFields :: (String -> String) -> [Con] -> [Field ([Context], Subst)]
groupFields mk
  = map (rename mk)
  . concatMap (\fs -> let vals  = concat (toList <$> fs)
                          cons  = fst <$> vals
                          subst = concat (snd <$> vals)
                       in nub (fmap (const (cons, subst)) <$> fs)
              )
  . groupBy eq
  . sortBy (comparing name)
  . concatMap constructorFields
  where name (Field n _ _ _) = n
        eq f g = False `fromMaybe` ((==) <$> name f <*> name g)
        rename f (Field n a b c) =
          Field (mkName . f . nameBase <$> n) a b c

constructorFields :: Con -> [Field (Context, Subst)]
constructorFields con =

  case con of

    NormalC c fs -> one <$> zip [0..] fs
      where one (i, f@(_, ty)) = Field Nothing mono ty (Context i c con, [])
              where fsTys = map (typeVariables . snd) (delete f fs)
                    mono  = any (\x -> any (elem x) fsTys) (typeVariables ty)

    RecC c fs -> one <$> zip [0..] fs
      where one (i, f@(n, _, ty)) = Field (Just n) mono ty (Context i c con, [])
              where fsTys = map (typeVariables . trd) (delete f fs)
                    mono  = any (\x -> any (elem x) fsTys) (typeVariables ty)
                    trd (_, _, x) = x

    InfixC a c b -> one <$> [(0, a), (1, b)]
      where one (i, (_, ty)) = Field Nothing mono ty (Context i c con, [])
              where fsTys = map (typeVariables . snd) [a, b]
                    mono  = any (\x -> any (elem x) fsTys) (typeVariables ty)

    ForallC x y v -> setEqs <$> constructorFields v
      where eqs = [ (a, b) | EqualP a b <- y ]
            setEqs (Field a b c d) = Field a b c (first upd . second (eqs ++) $ d)
            upd (Context a b c) = Context a b (ForallC x y c)

prune :: [Context] -> [Con] -> [Con]
prune contexts allCons =
  case contexts of
    (Context _ _ con) : _
       -> filter (unifiableCon con) allCons
    [] -> []

unifiableCon :: Con -> Con -> Bool
unifiableCon a b = and (zipWith unifiable (indices a) (indices b))
  where indices con =
          case con of
            NormalC {}    -> []
            RecC    {}    -> []
            InfixC  {}    -> []
            ForallC _ x _ -> [ c | EqualP _ c <- x ]

unifiable :: Type -> Type -> Bool
unifiable x y =
  case (x, y) of
    ( VarT _        ,      _        ) -> True
    ( _             , VarT _        ) -> True
    ( AppT a b      , AppT c d      ) -> unifiable a c && unifiable b d
    ( SigT t k      , SigT s j      ) -> unifiable t s && k == j
    ( ForallT _ _ t , ForallT _ _ s ) -> unifiable t s
    ( a             , b             ) -> a == b

generateLabel
  :: Bool
  -> Bool
  -> Name
  -> [TyVarBndr]
  -> [Con]
  -> Field ([Context], Subst)
  -> Q Label

generateLabel failing concrete datatype dtVars allCons
              field@(Field name forcedMono fieldtype (contexts, subst)) =

  do let total = length contexts == length (prune contexts allCons)

     (Typing mono tyI tyO vars)
        <- computeTypes forcedMono fieldtype datatype dtVars subst

     let cat     = varT (mkName "cat")
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
                   then if mono
                        then [t| Mono.Lens Total $tyI $tyO |]
                        else [t| Poly.Lens Total $tyI $tyO |]
                   else if mono
                        then [t| Mono.Lens $partial $tyI $tyO |]
                        else [t| Poly.Lens $partial $tyI $tyO |]
         ty      = if concrete
                   then concTy
                   else if mono
                        then [t| Mono.Lens $cat $tyI $tyO |]
                        else [t| Poly.Lens $cat $tyI $tyO |]

     tvs <- nub . binderFromType <$> ty
     return $
       case name of
         Nothing -> LabelExpr tvs cont ty body
         Just n  ->

#if MIN_VERSION_template_haskell(2,8,0)
           -- Generate an inline declaration for the label.
           -- Type of InlineSpec removed in TH-2.8.0 (GHC 7.6)
           let inline = InlineP n Inline FunLike (FromPhase 0)
#else
           let inline = InlineP n (InlineSpec True True (Just (True, 0)))
#endif
            in LabelDecl n (return (PragmaD inline)) tvs cont ty body

-- Build a total polymorphic modification function from a getter and setter.

modifier :: ArrowApply cat => cat f o -> cat (i, f) g -> cat (cat o i, f) g
modifier g m = m . first app . arr (\(n, (f, o)) -> ((n, o), f)) . second (id &&& g)
{-# INLINE modifier #-}

-------------------------------------------------------------------------------

getter :: Bool -> Field ([Context], Subst) -> Q Exp
getter total (Field mn _ _ (cons, _)) =
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

setter :: Bool -> Field ([Context], Subst) -> Q Exp
setter total (Field mn _ _ (cons, _)) =
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

computeTypes :: Bool -> Type -> Name -> [TyVarBndr] -> Subst -> Q Typing
computeTypes forcedMono fieldtype datatype dtVars_ subst =

  do let fieldVars = typeVariables fieldtype
         tyO       = return fieldtype
         dtTypes   = substitute subst . typeFromBinder <$> dtVars_
         dtBinders = concatMap binderFromType dtTypes
         varNames  = nameFromBinder <$> dtBinders
         usedVars  = filter (`elem` fieldVars) varNames
         tyI       = return $ foldr (flip AppT) (ConT datatype) (reverse dtTypes)
         pretties  = mapTyVarBndr pretty <$> dtBinders
         mono      = forcedMono || isMonomorphic fieldtype dtBinders

     if mono
       then return $ Typing
               mono
               (prettyType <$> tyI)
               (prettyType <$> tyO)
               (nub pretties)
       else
         do let names = return <$> ['a'..'z']
                used  = show . pretty <$> varNames
                free  = filter (not . (`elem` used)) names
            subs <- forM (zip usedVars free) (\(a, b) -> (,) a <$> newName b)
            let rename = mapTypeVariables (\a -> a `fromMaybe` lookup a subs)

            return $ Typing
              mono
              (prettyType <$> [t| $tyI -> $(rename <$> tyI) |])
              (prettyType <$> [t| $tyO -> $(rename <$> tyO) |])
              (nub (pretties ++ map (mapTyVarBndr pretty) (PlainTV . snd <$> subs)))

isMonomorphic :: Type -> [TyVarBndr] -> Bool
isMonomorphic field vars =
  let fieldVars = typeVariables field
      varNames  = nameFromBinder <$> vars
      usedVars  = filter (`elem` fieldVars) varNames
   in null usedVars

-------------------------------------------------------------------------------
-- Generic helper functions dealing with Template Haskell

typeVariables :: Type -> [Name]
typeVariables = map nameFromBinder . binderFromType

typeFromBinder :: TyVarBndr -> Type
typeFromBinder (PlainTV  tv     ) = VarT tv
typeFromBinder (KindedTV tv kind) = SigT (VarT tv) kind

binderFromType :: Type -> [TyVarBndr]
binderFromType = go
  where
  go ty =
    case ty of
      ForallT ts _ _ -> ts
      AppT a b       -> go a ++ go b
      SigT t _       -> go t
      VarT n         -> [PlainTV n]
      _              -> []

mapTypeVariables :: (Name -> Name) -> Type -> Type
mapTypeVariables f = go
  where
  go ty =
    case ty of
      ForallT ts a b -> ForallT (mapTyVarBndr f <$> ts)
                                (mapPred f <$> a) (go b)
      AppT a b       -> AppT (go a) (go b)
      SigT t a       -> SigT (go t) a
      VarT n         -> VarT (f n)
      t              -> t

mapType :: (Type -> Type) -> Type -> Type
mapType f = go
  where
  go ty =
    case ty of
      ForallT v c t -> f (ForallT v c (go t))
      AppT a b      -> f (AppT (go a) (go b))
      SigT t k      -> f (SigT (go t) k)
      _             -> f ty

substitute :: Subst -> Type -> Type
substitute env = mapType sub
  where sub v = case lookup v env of
                  Nothing -> v
                  Just w  -> w

nameFromBinder :: TyVarBndr -> Name
nameFromBinder (PlainTV  n  ) = n
nameFromBinder (KindedTV n _) = n

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

-- Reify a name into a declaration.

reifyDec :: Name -> Q Dec
reifyDec name =
  do info <- reify name
     case info of
       TyConI dec -> return dec
       _ -> fclError "Info must be type declaration type."

-- Throw a fclabels specific error.

fclError :: String -> a
fclError err = error ("Data.Label.Derive: " ++ err)

