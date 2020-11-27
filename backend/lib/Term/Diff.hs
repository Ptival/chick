{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Term.Diff
  ( TermDiff,
    TermXFamily (..),
  )
where

import Bound.Name (Name (..), name)
import Data.Default (Default (..))
import Data.Function (on)
import Data.Generic.Diff (Cons (CCons), Nil (CNil))
import qualified Data.Generic.Diff as Diff
import Term.Binder (Binder (unBinder))
import qualified Term.Term as Term
import qualified Term.Universe as U
import Term.Variable ()

type TermDiff α = Diff.EditScript (TermXFamily α) (Term.TermX α Term.Variable) (Term.TermX α Term.Variable)

data TermXFamily α :: * -> * -> * where
  Annot' ::
    TermXFamily
      α
      (Term.TermX α Term.Variable)
      (Diff.Cons (Term.TermX α Term.Variable) (Diff.Cons (Term.TypeX α Term.Variable) Diff.Nil))
  App' ::
    TermXFamily
      α
      (Term.TermX α Term.Variable)
      (Diff.Cons (Term.TermX α Term.Variable) (Diff.Cons (Term.TermX α Term.Variable) Diff.Nil))
  Hole' :: TermXFamily α (Term.TermX α Term.Variable) Diff.Nil
  Lam' ::
    TermXFamily
      α
      (Term.TermX α Term.Variable)
      (Diff.Cons (Term.ScopedTerm (Term.TermX α) Term.Variable) Diff.Nil)
  Let' ::
    TermXFamily
      α
      (Term.TermX α Term.Variable)
      (Diff.Cons (Term.TermX α Term.Variable) (Diff.Cons (Term.ScopedTerm (Term.TermX α) Term.Variable) Diff.Nil))
  Pi' ::
    TermXFamily
      α
      (Term.TermX α Term.Variable)
      (Diff.Cons (Term.TypeX α Term.Variable) (Diff.Cons (Term.ScopedTerm (Term.TypeX α) Term.Variable) Diff.Nil))
  Type' :: TermXFamily α (Term.TermX α Term.Variable) (Diff.Cons U.Universe Diff.Nil)
  Var' :: TermXFamily α (Term.TermX α Term.Variable) (Diff.Cons Term.Variable Diff.Nil)
  Scope' ::
    TermXFamily
      α
      (Term.ScopedTerm (Term.TermX α) Term.Variable)
      (Diff.Cons (Name Term.Variable ()) (Diff.Cons (Term.TermX α Term.Variable) Diff.Nil))
  Name' :: Name Term.Variable () -> TermXFamily α (Name Term.Variable ()) Diff.Nil
  Binder' :: Binder Term.Variable -> TermXFamily α (Binder Term.Variable) Diff.Nil
  Variable' :: Term.Variable -> TermXFamily α Term.Variable Diff.Nil
  Universe' :: U.Universe -> TermXFamily α U.Universe Diff.Nil

instance
  (Default α, Eq α, Show α) =>
  Diff.Family (TermXFamily α)
  where
  decEq Annot' Annot' = Just (Diff.Refl, Diff.Refl)
  decEq App' App' = Just (Diff.Refl, Diff.Refl)
  decEq Hole' Hole' = Just (Diff.Refl, Diff.Refl)
  decEq Lam' Lam' = Just (Diff.Refl, Diff.Refl)
  decEq Let' Let' = Just (Diff.Refl, Diff.Refl)
  decEq Pi' Pi' = Just (Diff.Refl, Diff.Refl)
  decEq Type' Type' = Just (Diff.Refl, Diff.Refl)
  decEq Var' Var' = Just (Diff.Refl, Diff.Refl)
  decEq Scope' Scope' = Just (Diff.Refl, Diff.Refl)
  decEq (Name' n1) (Name' n2)
    | on (==) name n1 n2 = Just (Diff.Refl, Diff.Refl)
    | otherwise = Nothing
  decEq (Binder' b1) (Binder' b2)
    | b1 == b2 = Just (Diff.Refl, Diff.Refl)
    | otherwise = Nothing
  decEq (Variable' v1) (Variable' v2)
    | v1 == v2 = Just (Diff.Refl, Diff.Refl)
    | otherwise = Nothing
  decEq _ _ = Nothing

  fields Annot' (Term.Annot _ t τ) = Just (CCons t (CCons τ CNil))
  fields App' (Term.App _ t1 t2) = Just (CCons t1 (CCons t2 CNil))
  fields Hole' (Term.Hole _) = Just CNil
  fields Lam' (Term.Lam _ bt) = Just (CCons bt CNil)
  fields Let' (Term.Let _ t1 bt2) = Just (CCons t1 (CCons bt2 CNil))
  fields Pi' (Term.Pi _ τ1 bτ2) = Just (CCons τ1 (CCons bτ2 CNil))
  fields Type' (Term.Type u) = Just (CCons u CNil)
  fields Var' (Term.Var _ v) = Just (CCons v CNil)
  fields Scope' s =
    let (b, t) = Term.unscopeTerm s
     in let n = case unBinder b of
              Nothing -> error "fields Scope' binder" -- Term.Variable "_"
              Just v -> v
         in Just (CCons (Name n ()) (CCons t CNil))
  fields (Name' _) _ = Just CNil
  fields (Binder' _) _ = Just CNil
  fields (Variable' _) _ = Just CNil
  fields _ _ = Nothing

  apply Annot' (CCons t (CCons τ CNil)) = Term.Annot def t τ
  apply App' (CCons t1 (CCons t2 CNil)) = Term.App def t1 t2
  apply Hole' CNil = Term.Hole def
  apply Lam' (CCons bt CNil) = Term.Lam def bt
  apply Let' (CCons t1 (CCons bt2 CNil)) = Term.Let def t1 bt2
  apply Pi' (CCons τ1 (CCons bτ2 CNil)) = Term.Pi def τ1 bτ2
  apply Type' (CCons u CNil) = Term.Type u
  apply Var' (CCons v CNil) = Term.Var def v
  apply Scope' (CCons n (CCons t CNil)) = Term.abstractVariable (name n) t
  --apply (Scope' s) (CCons t CNil) = s
  apply (Name' n) CNil = n
  apply (Binder' b) CNil = b
  apply (Variable' v) CNil = v
  apply (Universe' u) CNil = u

  string Annot' = "Annot"
  string App' = "App"
  string Hole' = "Hole"
  string Lam' = "Lam"
  string Let' = "Let"
  string Pi' = "Pi"
  string Type' = "Type"
  string Var' = "Var"
  string Scope' = "Scope"
  string (Name' n) = show n
  string (Binder' b) = show b
  string (Variable' v) = show v
  string (Universe' u) = show u

instance (Default α, Eq α, Show α) => Diff.Type (TermXFamily α) Term.Variable where
  constructors = [Diff.Abstr Variable']

instance (Default α, Eq α, Show α) => Diff.Type (TermXFamily α) (Binder Term.Variable) where
  constructors = [Diff.Abstr Binder']

instance (Default α, Eq α, Show α) => Diff.Type (TermXFamily α) (Name Term.Variable ()) where
  constructors = [Diff.Abstr Name']

instance (Default α, Eq α, Show α) => Diff.Type (TermXFamily α) U.Universe where
  constructors = [Diff.Abstr Universe']

instance
  ( Default α,
    Eq α,
    Show α,
    Diff.Type (TermXFamily α) (Name Term.Variable ())
  ) =>
  Diff.Type (TermXFamily α) (Term.ScopedTerm (Term.TermX α) Term.Variable)
  where
  constructors = [Diff.Concr Scope']

instance
  ( Default α,
    Eq α,
    Show α,
    Diff.Type (TermXFamily α) (Name Term.Variable ())
  ) =>
  Diff.Type (TermXFamily α) (Term.TermX α Term.Variable)
  where
  constructors =
    [ Diff.Concr Annot',
      Diff.Concr App',
      Diff.Concr Hole',
      Diff.Concr Lam',
      Diff.Concr Let',
      Diff.Concr Pi',
      Diff.Concr Type',
      Diff.Concr Var'
    ]

{-
instance
  (ForallX Default α, Type (TermXFamily α ν) ν) =>
  Type (TermXFamily α ν) (Term.TermX α (Var () (Term.TermX α ν))) where
    constructors =
      [ Concr Annot'
      , Concr   App'
      , Concr  Hole'
      , Concr   Lam'
      , Concr   Let'
      , Concr    Pi'
      , Concr  Type'
      , Concr   Var'
      ]
-}
