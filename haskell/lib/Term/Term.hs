-- {-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveFoldable #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language DeriveTraversable #-}
-- {-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
-- {-# language GADTs #-}
{-# language LambdaCase #-}
-- {-# language MultiParamTypeClasses #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
-- {-# language TypeFamilies #-}
-- {-# language TypeOperators #-}
-- {-# language UndecidableInstances #-}

module Term.Term where

import Bound
import Bound.Name
import Bound.Scope
import Control.Monad
import Control.Monad.Reader.Class
import Data.Bifunctor
import Data.Functor.Classes
import Data.Typeable
--import GHC.Exts                  (Constraint)
import GHC.Generics
import Text.PrettyPrint.Annotated.WL
import Text.Printf

import Precedence
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import PrettyPrinting.Utils
import Term.Binder
import Term.Variable

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

annotSymbol, holeSymbol :: String
annotSymbol = "@"
holeSymbol  = "?"

type NameScope = Scope (Name Variable ())

data TermX α ν
  = Annot α (TermX α ν) (TypeX α ν)
  | App   α (TermX α ν) (TermX α ν)
  | Hole  α
  | Lam   α             (NameScope (TermX α) ν)
  | Let   α (TermX α ν) (NameScope (TermX α) ν)
  | Pi    α (TypeX α ν) (NameScope (TypeX α) ν)
  | Type
  | Var   ν
  deriving
    ( Foldable
    , Functor
    , Generic
    , Traversable
    , Typeable
    )

type TypeX = TermX

instance Bifunctor TermX where
  bimap l r =
    let go = bimap l r in
    let bimapScope s = hoistScope (bimap l id) (r <$> s) in
    \case
      Annot a t  τ   -> Annot (l a) (go t)  (go τ)
      App   a t1 t2  -> App   (l a) (go t1) (go t2)
      Hole  a        -> Hole  (l a)
      Lam   a bt     -> Lam   (l a)         (bimapScope bt)
      Let   a t1 bt2 -> Let   (l a) (go t1) (bimapScope bt2)
      Pi    a τ1 bτ2 -> Pi    (l a) (go τ1) (bimapScope bτ2)
      Type           -> Type
      Var   v        -> Var   (r v)

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
      (Type,         Type)           -> True
      (Var v,        Var v')         -> eqVar v v'
      (_,            _)              -> False

instance (Eq ν) => Eq (TermX α ν) where
  Annot _ t τ == Annot _ t' τ'= t == t' && τ == τ'
  App _ t1 t2 == App _ t1' t2' = t1 == t1' && t2 == t2'
  Hole _ == Hole _ = True
  Lam _ bt == Lam _ bt' = bt == bt'
  Let _ t1 bt2 == Let _ t1' bt2' = t1 == t1' && bt2 == bt2'
  Pi _ τ1 bτ2 == Pi _ τ1' bτ2' = τ1 == τ1' && bτ2 == bτ2'
  Type == Type = True
  Var v == Var v' = v == v'
  _ == _ = False

--deriving instance (ForallX (Serial m) α, Monad m) => Serial m  (TermX α ν)
--deriving instance  ForallX Out        α           => Out       (TermX α ν)

instance Applicative (TermX α) where
  pure = Var
  (<*>) = ap

instance Monad (TermX α) where
  return = Var
  Annot a t  τ   >>= f = Annot a (t   >>= f) (τ  >>= f)
  App   a t1 t2  >>= f = App   a (t1  >>= f) (t2 >>= f)
  Hole  a        >>= _ = Hole  a
  Lam   a bt     >>= f = Lam   a             (bt  >>>= f)
  Let   a t1 bt2 >>= f = Let   a (t1  >>= f) (bt2 >>>= f)
  Pi    a τ1 bτ2 >>= f = Pi    a (τ1  >>= f) (bτ2 >>>= f)
  Type           >>= _ = Type
  Var   v        >>= f = f v

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
  Pi    a _ _ -> Just a
  Type        -> Nothing -- removed annotation because it makes TypeChecked infinite
  Var     _   -> Nothing -- removed annotation to make TermX applicative

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

unscopeTerm :: Scope (Name Variable ()) (TermX ξ) Variable -> (Binder Variable, TermX ξ Variable)
unscopeTerm t =
  let Name n _ : _ = bindings t in
  let b = if unVariable n == "_" then Nothing else Just n in
  (Binder b, instantiate1Name (Var n) t)

instance PrettyPrintableUnannotated (TermX ξ) where
  prettyDocU t = do
    precs <- ask
    return $ par precs (PrecMin, TolerateEqual) . prettyTermDocPrec precs $ t

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
        Pi    a τ1 bτ2 -> showString "Pi ("    . shows a . showString ") (" . liftShowsPrec sP sL p τ1 . showString ") (" . liftShowsPrec sP sL p bτ2 . showString ")"
        Type           -> showString "Type"
        Var   v        -> showString "Var (" . sP p v . showString ")"

instance (PrintfArg α, Show α) => PrintfArg (TermX α Variable) where
  formatArg t = formatString (show t)

prettyTermDocPrec ::
  forall a ξ. PrecedenceTable -> TermX ξ Variable -> (Doc a, Precedence)
prettyTermDocPrec precs = goTerm

  where

    go :: (Precedence, Tolerance) -> TermX ξ Variable -> Doc a
    go pt = par precs pt . prettyTermDocPrec precs

    goTerm :: TermX ξ Variable -> (Doc a, Precedence)
    goTerm = \case

      Annot _ t τ ->
        (fillSep
         [ go (PrecAnnot, TolerateHigher) t
         , text annotSymbol
         , go (PrecAnnot, TolerateHigher) τ
         ]
        , PrecAnnot)

      App _ t1 t2 ->
        (fillSep
         [ go (PrecApp, TolerateEqual) t1
         , go (PrecApp, TolerateHigher) t2
         ]
        , PrecApp)

      Hole _ -> (text holeSymbol, PrecAtom)

      l@(Lam _ _) -> (goLams [] l, PrecLam)

      Let _ t1 bt2 ->
        let [Name n ()] = bindings bt2 in
        (fillSep
         [ text "let"
         , prettyDoc n
         , char '='
         , go (PrecMin, TolerateEqual) t1
         , text "in"
         , go (PrecLet, TolerateEqual) (instantiate1Name (Var n) bt2)
         ]
        , PrecLet)

      Pi _ τ1 bτ2 ->
        let [Name n ()] = bindings bτ2 in
          case unVariable n of
            "_" ->
              (fillSep
               [ go (PrecArrow, TolerateHigher) τ1
               , char '→'
               , go (PrecArrow, TolerateEqual) (instantiate1Name (Var n) bτ2)
               ]
              , PrecArrow)
            _ ->
              (fillSep
               [ parens $ fillSep
                 [ prettyDoc n
                 , char ':'
                 , go (PrecMin, TolerateEqual) τ1
                 ]
               , char '→'
               , go (PrecArrow, TolerateEqual) (instantiate1Name (Var n) bτ2)
               ]
              , PrecArrow)

      Type -> (text "Type", PrecAtom)

      Var v -> (prettyDoc v, PrecAtom)

    goLams :: [Doc a] -> TermX ξ Variable -> Doc a
    goLams l = \case
      Lam _ bt ->
        let [Name n ()] = bindings bt in
        goLams (prettyDoc n : l) (instantiate1Name (Var n) bt)
      t -> fillSep
          [ char 'λ'
          , fillSep . reverse $ l
          , char '.'
          , go (PrecMin, TolerateEqual) t
          ]