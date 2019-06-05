{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Term.Term
  ( module Term.Binder
  , module Term.Variable
  , Branch(..)
  , GuardAndBody(..)
  , NamesScopeT
  , ScopedTerm(..)
  , TermX(..)
  , TypeX
  , UnsupportedOCamlTerm(..)
  , abstractAnonymous
  , abstractBinder
  , abstractVariable
  , annotateHead
  , annotationOf
  , annotSymbol
  , arrowSymbol
  , scopedTerm
  , forallSymbol
  -- , getName
  , holeSymbol
  , lamSymbol
  , originalBinder
  , originalVariable
  , packBranch
  , postForallSymbol
  , postLamSymbol
--  , rAnnot
--  , rApp
--  , rHole
--  , rLam
--  , rLet
--  , rPi
--  , rType
--  , rVar
  , simultaneousSubstitute
  , substitute
  , unpackBranch
  , unsafeTermToNum
  , unscopeNames
  , unscopeTerm
  , wildcardSymbol
  ) where

import Bound ((>>>=))
import Bound.Class
import Bound.Name
import Bound.Scope
import Bound.ScopeT
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Functor.Classes
import Data.Functor.Compose
import Data.List
import Data.Maybe
import Data.String
import Data.Typeable
import GHC.Generics
import Language.OCaml.Definitions.Parsing.ParseTree
import Text.Printf

import Term.Binder
import Term.Variable
import Term.Universe (Universe)

{-
The type-ascription symbol unfortunately cannot be ":" like you would expect,
because in the presence of the four syntactic constructs:

`(t)`
`t → t`
`t : t`
`(t : t) → t`

The string `(t : t) → t` can be decomposed in two non-equivalent ways.

One could either:
- change the Pi syntax, for instance `[t : t] → t`, but people would be surprised by
  the meaning of `(t : t) → t`
- change the Pi syntax, for instance `Π (t : t) → t`, but people would be surprised by
  the meaning of `(t : t) → t`

-}

annotSymbol, arrowSymbol, forallSymbol, holeSymbol :: String
lamSymbol, postForallSymbol, postLamSymbol, wildcardSymbol :: String
annotSymbol = ":"
arrowSymbol = "→"
forallSymbol = "∀"
holeSymbol  = "_"
lamSymbol = "λ"
postForallSymbol = ","
postLamSymbol = ","
wildcardSymbol = "_"

data ScopedTerm f a = ScopedTerm
  { _scopedTerm :: Scope (Name Variable ()) f a
  , _originalBinder :: Binder Variable
  }
  deriving
    ( Foldable
    , Functor
    , Generic
    , Traversable
    , Typeable
    )
makeLenses ''ScopedTerm

originalVariable :: ScopedTerm f a -> Variable
originalVariable bt = case unBinder $ view originalBinder bt of
  Nothing -> error "originalVariable: Nothing"
  Just v -> v

-- abstractAnonymous :: (Monad f) => f ν -> Scope (Name ν ()) f ν
-- abstractAnonymous = abstractName (const Nothing)

-- abstractBinder :: (Monad f) => Eq ν => Binder ν -> f ν -> Scope (Name ν ()) f ν
-- abstractBinder b =
--   case unBinder b of
--     Nothing -> abstractAnonymous
--     Just v  -> abstract1Name v

-- abstractVariable :: (Monad f) => Eq ν => ν -> f ν -> Scope (Name ν ()) f ν
-- abstractVariable = abstract1Name

abstractAnonymous :: TermX α Variable -> ScopedTerm (TermX α) Variable
abstractAnonymous = abstractBinder (Binder Nothing)

abstractVariable :: Variable -> TermX α Variable -> ScopedTerm (TermX α) Variable
abstractVariable v = abstractBinder (Binder (Just v))

abstractBinder :: Binder Variable -> TermX α Variable -> ScopedTerm (TermX α) Variable
abstractBinder b t = ScopedTerm
  { _scopedTerm = case unBinder b of
                 Nothing -> abstractName (const Nothing) t
                 Just v -> abstract1Name v t
  , _originalBinder = b
  }

type MkNamesScope ν = Scope (Name ν Int)

-- type NamesScope = Scope (Name Variable Int)

type MkNamesScopeT ν = ScopeT (Name ν Int)

type NamesScopeT = ScopeT (Name Variable Int)

-- type MkNameScope ν f a = (Binder ν, Scope (Name ν ()) f a)

-- type NameScope f a = (Binder Variable, Scope (Name Variable ()) f a)

data GuardAndBody f ν = GuardAndBody
  { branchGuard :: Maybe (f ν)
  , branchBody  :: f ν
  }
  deriving
    ( Foldable
    , Functor
    , Generic
    , Traversable
    , Typeable
    )

dispatchGuardAndBody :: (f v -> g w) -> GuardAndBody f v -> GuardAndBody g w
dispatchGuardAndBody f gb = GuardAndBody
    { branchGuard = f <$> branchGuard gb
    , branchBody  = f  $  branchBody  gb
    }

instance Bound GuardAndBody where
  gb >>>= f = dispatchGuardAndBody (>>= f) gb

instance Applicative f => Applicative (GuardAndBody f) where
  pure v = GuardAndBody Nothing (pure v) -- does this even make sense?
  f <*> x = GuardAndBody
    { branchGuard = ((<*>) <$> branchGuard f) <*> branchGuard x
    , branchBody  = branchBody  f <*> branchBody  x
    }

instance Eq1 f => Eq1 (GuardAndBody f) where
  liftEq eqVar (GuardAndBody g b) (GuardAndBody g' b') =
    liftEq (liftEq eqVar) g g' && liftEq eqVar b b'

instance (Eq1 f, Eq ν) => Eq (GuardAndBody f ν) where
  gb == gb' = liftEq (==) gb gb'

instance ToJSON (f Variable) => ToJSON (GuardAndBody f Variable) where
  toJSON gb =
    object
    [ "branchBody"  .= branchBody gb
    , "branchGuard" .= branchGuard gb
    ]

data Branch α ν = Branch
  { branchConstructor  :: Variable -- do not put ν here
  -- as this field cannot be term-substituted
  , branchNbArguments  :: Int
  , branchGuardAndBody :: NamesScopeT GuardAndBody (TermX α) ν
  }
  deriving
    ( Foldable
    , Functor
    , Generic
    , Traversable
    , Typeable
    )

instance Eq1 (Branch α) where
  liftEq eqVar (Branch c n b) (Branch c' n' b') =
    c == c' && n == n'
    && liftEq eqVar b b'
    -- && liftEq (liftEq (liftEq eqVar)) (unscope b) (unscope b')

instance (Eq ν) => Eq (Branch α ν) where
  term1 == term2 = liftEq (==) term1 term2

instance ToJSON α => ToJSON (Branch α Variable) where
  toJSON b =
    let (ctor, parameters, body) = unpackBranch b in
    object
    [ "branchConstructor" .= ctor
    , "branchParameters"  .= parameters
    , "branchBody"        .= body
    ]

data UnsupportedOCamlTerm
  = UnsupportedExpression Expression
  | UnsupportedCoreType   CoreType
  deriving
    ( Eq
    , Generic
    , Show
    , Typeable
    )

data TermX α ν
  = Annot α (TermX α ν) (TypeX α ν)
  | App   α (TermX α ν) (TermX α ν)
  | Hole  α
  | Lam   α             (ScopedTerm (TermX α) ν)
  | Let   α (TermX α ν) (ScopedTerm (TermX α) ν)
  | Match α (TermX α ν) [Branch α ν]
  | Pi    α (TypeX α ν) (ScopedTerm (TypeX α) ν)
  | Type  Universe
  -- it's really annoying not to have annotations on Var when type-checking
  -- but having α makes it not an applicative (can't come up with arbitrary α)
  -- so (Maybe α) lets us put stuff there when handy, but not always
  | Var   (Maybe α) ν
  | UnsupportedOCaml UnsupportedOCamlTerm
  deriving
    ( Foldable
    , Functor
    , Generic
    , Traversable
    , Typeable
    )

type TypeX = TermX

-- $(makeBoomerangs ''TermX)

instance Bifunctor Branch where
  bimap :: ∀ a b c d. (a -> b) -> (c -> d) -> Branch a c -> Branch b d
  bimap l r (Branch a b c) = Branch a b (bimapGB c)
    where
      bimapGB :: NamesScopeT GuardAndBody (TermX a) c -> NamesScopeT GuardAndBody (TermX b) d
      bimapGB s = hoistScopeT (dispatchGuardAndBody (bimap l id)) (bimap l id) $ r <$> s
      -- bimapGB :: NamesScope (GuardAndBody (TermX a)) c -> NamesScope (GuardAndBody (TermX b)) d
      -- bimapGB s = hoistScope (dispatchGuardAndBody (bimap l id)) $ r <$> s

instance Bifunctor TermX where
  bimap l r =
    \case
      Annot a t  τ   -> Annot (l a) (go t)  (go τ)
      App   a t1 t2  -> App   (l a) (go t1) (go t2)
      Hole  a        -> Hole  (l a)
      Lam   a bt     -> Lam   (l a)         (over scopedTerm bimapScope bt)
      Let   a t1 bt2 -> Let   (l a) (go t1) (over scopedTerm bimapScope bt2)
      Match a d bs   -> Match (l a) (go d)  (bimap l r <$> bs)
      Pi    a τ1 bτ2 -> Pi    (l a) (go τ1) (over scopedTerm bimapScope bτ2)
      Type  u        -> Type  u
      Var   a v      -> Var   (l <$> a) (r v)
      UnsupportedOCaml o -> UnsupportedOCaml o
    where
      go = bimap l r
      bimapScope s = hoistScope (bimap l id) (r <$> s)

instance Eq1 (TermX α) where
  liftEq eqVar term1 term2 =
    let (===) = liftEq eqVar in
    case (term1, term2) of
      (Annot _ t τ,  Annot _ t' τ')  -> t === t' && τ === τ'
      (Annot _ _ _, _) -> False
      (App _ t1 t2,  App _ t1' t2')  -> t1 === t1' && t2 === t2'
      (App _ _ _, _) -> False
      (Hole _,       Hole _)         -> True
      (Hole _, _) -> False
      (Lam _ bt,     Lam _ bt')      -> liftEq eqVar (view scopedTerm bt) (view scopedTerm bt')
      (Lam _ _, _) -> False
      (Let _ t1 bt2, Let _ t1' bt2') ->
        t1 === t1' && liftEq eqVar (view scopedTerm bt2) (view scopedTerm bt2')
      (Let _ _ _, _) -> False
      (Match _ d bs, Match _ d' bs') -> d === d' && liftEq (liftEq eqVar) bs bs'
      (Match _ _ _, _) -> False
      (Pi _ τ1 bτ2,  Pi _ τ1' bτ2')  ->
        τ1 === τ1' && liftEq eqVar (view scopedTerm bτ2) (view scopedTerm bτ2')
      (Pi _ _ _, _) -> False
      (Type u,       Type u')        -> u == u'
      (Type _, _) -> False
      (Var _ v,      Var _ v')       -> eqVar v v'
      (Var _ _, _) -> False
      (UnsupportedOCaml o, UnsupportedOCaml o') -> o == o'
      (UnsupportedOCaml _, _) -> False

instance (Eq ν) => Eq (TermX α ν) where
  term1 == term2 = liftEq (==) term1 term2

--deriving instance (ForallX (Serial m) α, Monad m) => Serial m  (TermX α ν)
--deriving instance  ForallX Out        α           => Out       (TermX α ν)

instance Applicative (TermX α) where
  pure = Var Nothing
  (<*>) = ap

instance Monad (TermX α) where
  return = Var Nothing
  (>>=) :: ∀ a b. TermX α a -> (a -> TermX α b) -> TermX α b
  Annot a t  τ       >>= f = Annot a (t  >>= f) (τ  >>= f)
  App   a t1 t2      >>= f = App   a (t1 >>= f) (t2 >>= f)
  Hole  a            >>= _ = Hole  a
  Lam   a bt         >>= f = Lam   a            (over scopedTerm (>>>= f) bt)
  Let   a t1 bt2     >>= f = Let   a (t1 >>= f) (over scopedTerm (>>>= f) bt2)
  Match a d bs       >>= f = Match a (d  >>= f) (map bindBranch bs)
    where
      bindBranch :: Branch α a -> Branch α b
      bindBranch b = Branch
        { branchConstructor  = branchConstructor b
        , branchNbArguments  = branchNbArguments b
        , branchGuardAndBody = branchGuardAndBody b >>>>= f
        }
  Pi    a τ1 bτ2     >>= f = Pi a (τ1 >>= f) (over scopedTerm (>>>= f) bτ2)
  Type  u            >>= _ = Type u
  Var   _ v          >>= f = f v
  UnsupportedOCaml o >>= _ = UnsupportedOCaml o

instance (ToJSON α) => ToJSON (TermX α Variable) where
  toJSON = \case

    Annot _a t τ -> object
      [ "tag"  .= ("Annot" :: String)
      , "term" .= t
      , "type" .= τ
      ]

    App _a t1 t2 -> object
      [ "tag"   .= ("App" :: String)
      , "left"  .= t1
      , "right" .= t2]

    Hole _a -> object
      [ "tag" .= ("Hole" :: String) ]

    Lam _a bt ->
      let (b, t) = unscopeTerm bt in
      object
      [ "tag"    .= ("Lam" :: String)
      , "binder" .= b
      -- TODO: originalBinder?
      , "body"   .= t
      ]

    Let _a t1 bt2 ->
      let (b, t2) = unscopeTerm bt2 in
      object
      [ "tag"    .= ("Let" :: String)
      , "binder" .= b
      , "t1"     .= t1
      , "t2"     .= t2
      ]

    Match _a _d _bs -> object
      [ "tag" .= ("Match" :: String) ]

    Pi _a τ1 bτ2 ->
      let (b, τ2) = unscopeTerm bτ2 in
      object
      [ "tag"    .= ("Pi" :: String)
      , "binder" .= b
      , "τ1"     .= τ1
      , "τ2"     .= τ2
      ]

    Type u -> object
      [ "tag"      .= ("Type" :: String)
      , "universe" .= u
      ]

    Var _a v -> object
      [ "tag"      .= ("Var" :: String)
      , "variable" .= v
      ]

    UnsupportedOCaml _ -> object
      [ "tag"      .= ("UnsupportedOCaml" :: String)
      ]

--object ["name" .= name, "age" .= age]

--deriving instance Foldable (TermX α)
--deriving instance Traversable (TermX α)

{-
genTerm ::
  (Arbitrary ν, Default (X_Var α), ForallX Arbitrary α, CoArbitrary ν) =>
  Int -> Gen (TermX α ν)
genTerm 0 =
  frequency
  [ (1, Hole <$> arbitrary)
  , (1, Type <$> arbitrary)
  , (3, Var  <$> arbitrary)
  ]
genTerm n =
  let arbitrary' = choose (0, n-1) >>= genTerm in
  frequency
  [ (1, Annot <$> arbitrary <*> arbitrary' <*> arbitrary')
  , (3, App   <$> arbitrary <*> arbitrary' <*> arbitrary')
  --, Hole  <$> arbitrary
  , (3, Lam   <$> arbitrary <*> arbitrary)
  , (1, Let   <$> arbitrary <*> arbitrary <*> arbitrary)
  , (3, Pi    <$> arbitrary <*> arbitrary <*> arbitrary)
  --, Type  <$> arbitrary
  , (1, Var   <$> arbitrary)
  ]

instance (ForallX Arbitrary α, Arbitrary ν, CoArbitrary ν, Default (X_Var α)) =>
         Arbitrary (TermX α ν) where

  arbitrary = sized genTerm

  shrink = \case

    Annot a t τ ->
      [t, τ] ++ [Annot a' t' τ' | (a', t', τ') <- shrink (a, t, τ)]

    App a t1 t2 ->
      [t1, t2] ++ [App a' t1' t2' | (a', t1', t2') <- shrink (a, t1, t2)]

    Hole _ -> []

    Lam a bt ->
      -- [instantiate1 (Var _ "x") bt]
      -- ++
      [Lam a' bt' | (a', bt') <- shrink (a, bt)]

    Let a t1 bt2 ->
      -- [t1, unscope bt2]
      -- ++
      [Let a' t1' bt2' | (a', t1', bt2') <- shrink (a, t1, bt2)]

    Pi a τ bt ->
      -- [τ, unscope bt]
      -- ++
      [Pi a' τ' bt' | (a', τ', bt') <- shrink (a, τ, bt)]

    Type _ -> []

    Var v -> [Var v' | v' <- shrink v]

-}

--  liftShowsPrec = error "TODO"
--deriving instance (Show α, Show ν) => Show (TermX α ν)

{-
We can retrieve the annotation for a term generically only when they all
share the same annotation type. Otherwise, the output type would depend on
the constructor.
-}
annotationOf :: TermX α ν -> Maybe α
annotationOf = \case
  Annot a _ _ -> Just a
  App   a _ _ -> Just a
  Hole  a     -> Just a
  Lam   a _   -> Just a
  Let   a _ _ -> Just a
  Match a _ _ -> Just a
  Pi    a _ _ -> Just a
  Type  _     -> Nothing -- removed annotation because it makes TypeChecked infinite
  Var   a _   -> a -- maybe-d annotation to make TermX applicative
  UnsupportedOCaml _ -> Nothing

annotateHead :: α -> TermX β ν -> TermX α ν
annotateHead a = bimap (const a) id

substitute ::
  (Monad f, Eq a) => a -> f a -> f a -> f a
substitute v t1 t2 =
  t2 >>= \ b -> if b == v then t1 else return b

simultaneousSubstitute ::
  (Monad f, Eq a) => [(a, f a)] -> f a -> f a
simultaneousSubstitute l w =
  w >>= \ b -> case lookup b l of
                Just p -> p
                Nothing -> return b

mkVarAtIndexT :: (Foldable (t f)) => MkNamesScopeT v t f a -> Int -> Maybe v
mkVarAtIndexT s ndx = name <$> find ((==) ndx . snd . view _Name) (bindingsT s)

mkVarAtIndex :: (Foldable f) => MkNamesScope b f a -> Int -> Maybe b
mkVarAtIndex s ndx = name <$> find ((==) ndx . snd . view _Name) (bindings s)

unscopeNamesWithT :: (Bound t, Foldable (t f), Monad f) =>
  (ν1 -> f ν2) ->
  ScopeT (Name ν1 Int) t f ν2 ->
  (Int -> Binder ν1, t f ν2)
unscopeNamesWithT f s =
  let varAtIndex = mkVarAtIndexT s in
  (Binder <$> varAtIndex, instantiateNameT (f <$> fromJust <$> varAtIndex) s)

unscopeNamesWith :: (Foldable f, Monad f) =>
  (ν1 -> f ν2) ->
  Scope (Name ν1 Int) f ν2 ->
  (Int -> Binder ν1, f ν2)
unscopeNamesWith f s =
  let varAtIndex = mkVarAtIndex s in
  (Binder <$> varAtIndex, instantiateName (f <$> fromJust <$> varAtIndex) s)

unscopeNamesT :: (Bound t, Foldable (t f), Monad f) =>
  ScopeT (Name ν Int) t f ν ->
  (Int -> Binder ν, t f ν)
unscopeNamesT = unscopeNamesWithT pure

unscopeNames :: (Foldable f, Monad f) =>
  Scope (Name ν Int) f ν ->
  (Int -> Binder ν, f ν)
unscopeNames = unscopeNamesWith pure

unpackBranch ::
  Branch α Variable -> (Variable, [Binder Variable], GuardAndBody (TermX α) Variable)
unpackBranch (Branch ctor nbArgs sguardbody) = (ctor, args, guardbody)
  where
    (binderAtIndex, guardbody) = unscopeNamesT sguardbody
    args = map binderAtIndex [0..nbArgs-1]

packBranch ::
  (Variable, [Binder Variable], GuardAndBody (TermX α) Variable) -> Branch α Variable
packBranch (ctor, args, body) = Branch ctor nbArgs sbody
  where
    nbArgs = length args
    sbody = abstractNameT (indexIn args) body
    indexIn l v = Binder (Just v) `elemIndex` l

unscopeTerm :: ScopedTerm (TermX α) Variable -> (Binder Variable, TermX α Variable)
unscopeTerm bt =
  ( view originalBinder bt
  , instantiate1Name (Var Nothing (originalVariable bt)) (view scopedTerm bt)
  )

instance IsString (TermX α Variable) where
  fromString s = Var Nothing (fromString s)

-- instance (Show α, Show1 (TermX α)) => Show1 (GuardAndBody α) where
--   liftShowsPrec sP sL p gb =
--     let guard = branchGuard gb in
--     let body  = branchBody gb in
--     showString "GuardAndBody (" . liftShowsPrec _ sL p guard . showString ") (" . _ body . showString ")"

instance (Show α) => Show1 (TermX α) where
  liftShowsPrec sP sL p = go
    where
      go = \case
        Annot a t  τ   -> showString "Annot (" . shows a . showString ") (" . go t  . showString ") (" . go τ . showString ")"
        App   a t1 t2  -> showString "App ("   . shows a . showString ") (" . go t1 . showString ") (" . go t2 . showString ")"
        Hole  a        -> showString "Hole ("  . shows a . showString ")"
        Lam   a bt     -> showString "Lam ("   . shows a . showString ") (" . liftShowsPrec sP sL p (view scopedTerm bt) . showString ")"
        Let   a t1 bt2 -> showString "Let ("   . shows a . showString ") (" . liftShowsPrec sP sL p t1 . showString ") (" . liftShowsPrec sP sL p (view scopedTerm bt2) . showString ")"
        Match a d  _bs -> showString "Match (" . shows a . showString ") (" . go d . showString ") (FIXME)"
        Pi    a τ1 bτ2 -> showString "Pi ("    . shows a . showString ") (" . liftShowsPrec sP sL p τ1 . showString ") (" . liftShowsPrec sP sL p (view scopedTerm bτ2) . showString ")"
        Type  u        -> showString (show u)
        Var   a v      -> showString "Var (" . shows a . showString ") (" . sP p v . showString ")"
        UnsupportedOCaml _ -> showString "UnsupportedOCaml"

instance (Show1 f) => Show1 (GuardAndBody f) where
  liftShowsPrec sP sL p = go
    where
      go gb = showString "GuardAndBody (" . showsMaybe (branchGuard gb) . showString ") (" . liftShowsPrec sP sL p (branchBody gb) . showString ")"
      showsMaybe m = liftShowsPrec sP sL p (Compose m)

deriving instance (Show (f ν), Show ν) => Show (GuardAndBody f ν)
deriving instance (Show α, Show (TermX α ν), Show ν) => Show (Branch α ν)
deriving instance (Show α, Show ν) => Show (ScopedTerm (TermX α) ν)
deriving instance (Show α, Show ν) => Show (TermX α ν)

instance (PrintfArg α, Show α) => PrintfArg (TermX α Variable) where
  formatArg t = formatString (show t)

unsafeTermToNum :: Num a => TermX () Variable -> a
unsafeTermToNum (Var _ "O") = 0
unsafeTermToNum (App _ (Var _ "S") rest) = 1 + unsafeTermToNum rest
unsafeTermToNum _ = error "unsafeTermToNum: not a number"

instance Num (TermX () Variable) where
  a + b = fromInteger $ unsafeTermToNum a + unsafeTermToNum b
  a * b = fromInteger $ unsafeTermToNum a * unsafeTermToNum b
  abs a = fromInteger $ abs $ unsafeTermToNum a
  negate _ = error "Num for Term only works for positive numbers"

  signum (Var _ "O") = Var Nothing "O"
  -- this is weird, but we just need to check that it's a number and return 1
  signum n = (unsafeTermToNum n :: Int) `seq` App () (Var Nothing "S") (Var Nothing "O")

  fromInteger n | n < 0     = error "Num for Term only works for positive numbers"
                | n == 0    = Var Nothing "O"
                | otherwise = App () "S" (fromInteger (n - 1))

-- getName :: (Foldable t) => Scope (Name Variable ()) t a -> Variable
-- getName t =
--   let uniqueBindings = nub $ bindings t in
--   case uniqueBindings of
--     [] -> error "NOOOO" -- Variable "_"
--     [Name n ()] -> n
--     _ -> error $ printf "getName: more than one binding for %s" (show uniqueBindings)

-- prettyTermDocPrec ::
--   forall a α. PrecedenceTable -> TermX α Variable -> (Doc a, Precedence)
-- prettyTermDocPrec precs = goTerm

--   where

--     go :: (Precedence, Tolerance) -> TermX α Variable -> Doc a
--     go pt = par precs pt . prettyTermDocPrec precs

--     goTerm :: TermX α Variable -> (Doc a, Precedence)
--     goTerm = \case

--       Annot _ t τ ->
--         (fillSep
--          [ go (PrecAnnot, TolerateHigher) t
--          , text annotSymbol
--          , go (PrecAnnot, TolerateHigher) τ
--          ]
--         , PrecAnnot)

--       App _ t1 t2 ->
--         (fillSep
--          [ go (PrecApp, TolerateEqual) t1
--          , go (PrecApp, TolerateHigher) t2
--          ]
--         , PrecApp)

--       Hole _ -> (text holeSymbol, PrecAtom)

--       l@(Lam _ _) -> (goLams [] l, PrecLam)

--       Let _ t1 bt2 ->
--         let n = getName bt2 in
--         (fillSep
--          [ text "let"
--          , prettyDoc n
--          , char '='
--          , go (PrecMin, TolerateEqual) t1
--          , text "in"
--          , go (PrecLet, TolerateEqual) (instantiate1Name (Var Nothing n) bt2)
--          ]
--         , PrecLet)

--       Pi _ τ1 bτ2 ->
--         let n = getName bτ2 in
--           case unVariable n of
--             "_" ->
--               (fillSep
--                [ go (PrecArrow, TolerateHigher) τ1
--                , char '→'
--                , go (PrecArrow, TolerateEqual) (instantiate1Name (Var Nothing n) bτ2)
--                ]
--               , PrecArrow)
--             _ ->
--               (fillSep
--                [ parens $ fillSep
--                  [ prettyDoc n
--                  , char ':'
--                  , go (PrecMin, TolerateEqual) τ1
--                  ]
--                , char '→'
--                , go (PrecArrow, TolerateEqual) (instantiate1Name (Var Nothing n) bτ2)
--                ]
--               , PrecArrow)

--       Type -> (text "Type", PrecAtom)

--       Var _ v -> (prettyDoc v, PrecAtom)

--     goLams :: [Doc a] -> TermX α Variable -> Doc a
--     goLams l = \case
--       Lam _ bt ->
--         let n = getName bt in
--         goLams (prettyDoc n : l) (instantiate1Name (Var Nothing n) bt)
--       t -> fillSep
--           [ char 'λ'
--           , fillSep . reverse $ l
--           , char '.'
--           , go (PrecMin, TolerateEqual) t
--           ]
