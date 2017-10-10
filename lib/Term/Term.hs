-- {-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
-- {-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
-- {-# language GADTs #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
-- {-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
-- {-# language TemplateHaskell #-}
-- {-# language TypeFamilies #-}
-- {-# language TypeOperators #-}
-- {-# language UndecidableInstances #-}

module Term.Term
  ( module Term.Binder
  , module Term.Variable
  , Branch(..)
  , NameScope
  , TermX(..)
  , TypeX
  , abstractAnonymous
  , abstractBinder
  , abstractVariable
  , annotateHead
  , annotationOf
  , annotSymbol
  , arrowSymbol
  , forallSymbol
  , getName
  , holeSymbol
  , lamSymbol
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
  , unscopeNames
  , unscopeTerm
  , wildcardSymbol
  ) where

import Bound ((>>>=))
import Bound.Name
import Bound.Scope
import Control.Lens
import Control.Monad
-- import Data.Bifunctor
import Data.Functor.Classes
import Data.List
import Data.Maybe
import Data.String
import Data.Typeable
--import GHC.Exts                  (Constraint)
import GHC.Generics
--import Text.Boomerang.TH
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

type NameScope  = Scope (Name Variable ())
type NamesScope = Scope (Name Variable Int)

data Branch α ν = Branch Variable Int (NamesScope (TermX α) ν)
  deriving
    ( Foldable
    , Functor
    , Generic
    , Traversable
    , Typeable
    )

data TermX α ν
  = Annot α (TermX α ν) (TypeX α ν)
  | App   α (TermX α ν) (TermX α ν)
  | Hole  α
  | Lam   α             (NameScope (TermX α) ν)
  | Let   α (TermX α ν) (NameScope (TermX α) ν)
  | Match α (TermX α ν) [Branch α ν]
  | Pi    α (TypeX α ν) (NameScope (TypeX α) ν)
  | Type  Universe
  -- it's really annoying not to have annotations on Var when type-checking
  -- but having α makes it not an applicative (can't come up with arbitrary α)
  -- so (Maybe α) lets us put stuff there when handy, but not always
  | Var   (Maybe α) ν
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
  bimap l r (Branch a b c) = Branch a b (bimapScope c)
    where
      bimapScope s = hoistScope (bimap l id) (r <$> s)

instance Bifunctor TermX where
  bimap l r =
    \case
      Annot a t  τ   -> Annot (l a) (go t)  (go τ)
      App   a t1 t2  -> App   (l a) (go t1) (go t2)
      Hole  a        -> Hole  (l a)
      Lam   a bt     -> Lam   (l a)         (bimapScope bt)
      Let   a t1 bt2 -> Let   (l a) (go t1) (bimapScope bt2)
      Match a d bs   -> Match (l a) (go d)  (bimap l r <$> bs)
      Pi    a τ1 bτ2 -> Pi    (l a) (go τ1) (bimapScope bτ2)
      Type  u        -> Type  u
      Var   a v      -> Var   (l <$> a) (r v)
    where
      go = bimap l r
      bimapScope s = hoistScope (bimap l id) (r <$> s)

instance Eq1 (TermX α) where
  liftEq eqVar term1 term2 =
    let (===) = liftEq eqVar in
    case (term1, term2) of
      (Annot _ t τ,  Annot _ t' τ')  -> t === t' && τ === τ'
      (App _ t1 t2,  App _ t1' t2')  -> t1 === t1' && t2 === t2'
      (Hole _,       Hole _)         -> True
      (Lam _ bt,     Lam _ bt')      -> liftEq eqVar bt bt'
      (Let _ t1 bt2, Let _ t1' bt2') -> t1 === t1' && liftEq eqVar bt2 bt2'
      (Pi _ τ1 bτ2,  Pi _ τ1' bτ2')  -> τ1 === τ1' && liftEq eqVar bτ2 bτ2'
      (Type u,       Type u')        -> u == u'
      (Var _ v,      Var _ v')       -> eqVar v v'
      (_,            _)              -> False

instance (Eq ν) => Eq (TermX α ν) where
  Annot _ t τ    == Annot _ t' τ'    = t  == t'  && τ  == τ'
  App   _ t1 t2  == App   _ t1' t2'  = t1 == t1' && t2 == t2'
  Hole  _        == Hole  _          = True
  Lam   _ bt     == Lam   _ bt'      = bt == bt'
  Let   _ t1 bt2 == Let   _ t1' bt2' = t1 == t1' && bt2 == bt2'
  Pi    _ τ1 bτ2 == Pi    _ τ1' bτ2' = τ1 == τ1' && bτ2 == bτ2'
  Type  u        == Type  u'         = u == u'
  Var   _ v      == Var   _ v'       = v  == v'
  _              == _ = False

--deriving instance (ForallX (Serial m) α, Monad m) => Serial m  (TermX α ν)
--deriving instance  ForallX Out        α           => Out       (TermX α ν)

instance Applicative (TermX α) where
  pure = Var Nothing
  (<*>) = ap

instance Monad (TermX α) where
  return = Var Nothing
  Annot a t  τ   >>= f = Annot a (t   >>= f) (τ  >>= f)
  App   a t1 t2  >>= f = App   a (t1  >>= f) (t2 >>= f)
  Hole  a        >>= _ = Hole  a
  Lam   a bt     >>= f = Lam   a             (bt  >>>= f)
  Let   a t1 bt2 >>= f = Let   a (t1  >>= f) (bt2 >>>= f)
  Match a d  bs  >>= f = Match a (d >>= f)   (map bindBranch bs)
    where
      bindBranch (Branch ctor args sbody) = Branch ctor args (sbody >>>= f)
  Pi    a τ1 bτ2 >>= f = Pi    a (τ1  >>= f) (bτ2 >>>= f)
  Type  u        >>= _ = Type  u
  Var   _ v      >>= f = f v

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

abstractAnonymous :: (Monad f) => f ν -> Scope (Name ν ()) f ν
abstractAnonymous = abstractName (const Nothing)

abstractBinder :: (Monad f) => Eq ν => Binder ν -> f ν -> Scope (Name ν ()) f ν
abstractBinder b =
  case unBinder b of
    Nothing -> abstractAnonymous
    Just v  -> abstract1Name v

abstractVariable :: (Monad f) => Eq ν => ν -> f ν -> Scope (Name ν ()) f ν
abstractVariable = abstract1Name

mkVarAtIndex :: NamesScope (TermX α) Variable -> Int -> Maybe Variable
mkVarAtIndex s ndx = name <$> find ((==) ndx . snd . view _Name) (bindings s)

unscopeNames ::
  Scope (Name Variable Int) (TermX α) Variable ->
  (Int -> Binder Variable, TermX α Variable)
unscopeNames s =
  let varAtIndex = mkVarAtIndex s in
  (Binder <$> varAtIndex, instantiateName (Var Nothing <$> fromJust <$> varAtIndex) s)

unpackBranch :: Branch α Variable -> (Variable, [Binder Variable], TermX α Variable)
unpackBranch (Branch ctor nbArgs sbody) = (ctor, args, body)
  where
    (binderAtIndex, body) = unscopeNames sbody
    args = map binderAtIndex [0..nbArgs-1]

packBranch :: (Variable, [Binder Variable], TermX α Variable) -> Branch α Variable
packBranch (ctor, args, body) = Branch ctor nbArgs sbody
  where
    nbArgs = length args
    sbody = abstractName (indexIn args) body
    indexIn l v = Binder (Just v) `elemIndex` l

unscopeTerm :: Scope (Name Variable ()) (TermX α) Variable -> (Binder Variable, TermX α Variable)
unscopeTerm t =
  let n = getName t in
  let b = if unVariable n == "_" then Nothing else Just n in
  (Binder b, instantiate1Name (Var Nothing n) t)

instance IsString (TermX α Variable) where
  fromString s = Var Nothing (fromString s)

deriving instance (Show α, Show ν) => Show (Branch α ν)
deriving instance (Show α, Show ν) => Show (TermX α ν)

instance (Show α) => Show1 (TermX α) where
  liftShowsPrec sP sL p = go
    where
      go = \case
        Annot a t  τ   -> showString "Annot (" . shows a . showString ") (" . go t  . showString ") (" . go τ . showString ")"
        App   a t1 t2  -> showString "App ("   . shows a . showString ") (" . go t1 . showString ") (" . go t2 . showString ")"
        Hole  a        -> showString "Hole ("  . shows a . showString ")"
        Lam   a bt     -> showString "Lam ("   . shows a . showString ") (" . liftShowsPrec sP sL p bt . showString ")"
        Let   a t1 bt2 -> showString "Let ("   . shows a . showString ") (" . liftShowsPrec sP sL p t1 . showString ") (" . liftShowsPrec sP sL p bt2 . showString ")"
        Match a d  _bs -> showString "Match (" . shows a . showString ") (" . go d . showString ") (FIXME)"
        Pi    a τ1 bτ2 -> showString "Pi ("    . shows a . showString ") (" . liftShowsPrec sP sL p τ1 . showString ") (" . liftShowsPrec sP sL p bτ2 . showString ")"
        Type  u        -> showString (show u)
        Var   a v      -> showString "Var (" . shows a . showString ") (" . sP p v . showString ")"

instance (PrintfArg α, Show α) => PrintfArg (TermX α Variable) where
  formatArg t = formatString (show t)

getName :: Foldable t => Scope (Name Variable ()) t a -> Variable
getName t =
  case bindings t of
    Name n () : _ -> n
    _ -> Variable "_"

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
