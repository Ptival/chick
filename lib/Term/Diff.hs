{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}

module Term.Diff where

import Data.Default
import Data.Generic.Diff

import Term.Binder
import Term.Variable
import Term.Term

type TermDiff ξ = EditScript (TermXFamily ξ) (TermX ξ) (TermX ξ)

data TermXFamily ξ :: * -> * -> * where
  Annot' :: TermXFamily ξ (TermX ξ) (Cons (TermX ξ) (Cons (TypeX ξ) Nil))
  App'   :: TermXFamily ξ (TermX ξ) (Cons (TermX ξ) (Cons (TermX ξ) Nil))
  Hole'  :: TermXFamily ξ (TermX ξ) Nil
  Lam'   :: TermXFamily ξ (TermX ξ) (Cons Binder (Cons (TermX ξ) Nil))
  Let'   :: TermXFamily ξ (TermX ξ) (Cons Binder (Cons (TermX ξ) (Cons (TermX ξ) Nil)))
  Pi'    :: TermXFamily ξ (TermX ξ) (Cons Binder (Cons (TypeX ξ) (Cons (TypeX ξ) Nil)))
  Type'  :: TermXFamily ξ (TermX ξ) Nil
  Var'   :: TermXFamily ξ (TermX ξ) (Cons Variable Nil)
  Binder'   :: Binder   -> TermXFamily ξ Binder   Nil
  Variable' :: Variable -> TermXFamily ξ Variable Nil

instance ForallX Default ξ => Family (TermXFamily ξ) where

  decEq Annot' Annot' = Just (Refl, Refl)
  decEq   App'   App' = Just (Refl, Refl)
  decEq  Hole'  Hole' = Just (Refl, Refl)
  decEq   Lam'   Lam' = Just (Refl, Refl)
  decEq   Let'   Let' = Just (Refl, Refl)
  decEq    Pi'    Pi' = Just (Refl, Refl)
  decEq  Type'  Type' = Just (Refl, Refl)
  decEq   Var'   Var' = Just (Refl, Refl)
  decEq (Binder' b1) (Binder' b2)
    | b1 == b2 = Just (Refl, Refl)
    | otherwise = Nothing
  decEq (Variable' v1) (Variable' v2)
    | v1 == v2 = Just (Refl, Refl)
    | otherwise = Nothing
  decEq _ _ = Nothing

  fields Annot' (Annot _ t τ)     = Just (CCons t (CCons τ CNil))
  fields   App' (App   _ t1 t2)   = Just (CCons t1 (CCons t2 CNil))
  fields  Hole' (Hole  _)         = Just CNil
  fields   Lam' (Lam   _ b t)     = Just (CCons b (CCons t CNil))
  fields   Let' (Let   _ b t1 t2) = Just (CCons b (CCons t1 (CCons t2 CNil)))
  fields    Pi' (Pi    _ b τ1 τ2) = Just (CCons b (CCons τ1 (CCons τ2 CNil)))
  fields  Type' (Type  _)         = Just CNil
  fields   Var' (Var   _ v)       = Just (CCons v CNil)
  fields (Binder'   _) _ = Just CNil
  fields (Variable' _) _ = Just CNil
  fields _ _ = Nothing

  apply Annot' (CCons t (CCons τ CNil))             = Annot def t τ
  apply   App' (CCons t1 (CCons t2 CNil))           = App   def t1 t2
  apply  Hole' CNil                                 = Hole  def
  apply   Lam' (CCons b (CCons t CNil))             = Lam   def b t
  apply   Let' (CCons b (CCons t1 (CCons t2 CNil))) = Let   def b t1 t2
  apply    Pi' (CCons b (CCons τ1 (CCons τ2 CNil))) = Pi    def b τ1 τ2
  apply  Type' CNil                                 = Type  def
  apply   Var' (CCons v CNil)                       = Var   def v
  apply (Binder'   b) CNil = b
  apply (Variable' v) CNil = v

  string  Annot' = "Annot"
  string    App' = "App"
  string   Hole' = "Hole"
  string    Lam' = "Lam"
  string    Let' = "Let"
  string     Pi' = "Pi"
  string   Type' = "Type"
  string    Var' = "Var"
  string (Binder'   b) = show b
  string (Variable' v) = show v

instance ForallX Default ξ => Type (TermXFamily ξ) Variable where
    constructors = [ Abstr Variable' ]

instance ForallX Default ξ => Type (TermXFamily ξ) Binder where
    constructors = [ Abstr Binder' ]

instance ForallX Default ξ => Type (TermXFamily ξ) (TermX ξ) where
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
