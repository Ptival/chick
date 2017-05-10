{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}

module Term.Diff where

--import Bound
--import Bound.Name
import Data.Default
import Data.Generic.Diff

--import Term.Binder
--import Term.Variable
import Term.Term

type TermDiff ξ ν = EditScript (TermXFamily ξ ν) (TermX ξ ν) (TermX ξ ν)

data TermXFamily ξ ν :: * -> * -> * where
  Annot' :: TermXFamily ξ ν (TermX ξ ν) (Cons (TermX ξ ν) (Cons (TypeX ξ ν) Nil))
  App'   :: TermXFamily ξ ν (TermX ξ ν) (Cons (TermX ξ ν) (Cons (TermX ξ ν) Nil))
  Hole'  :: TermXFamily ξ ν (TermX ξ ν) Nil
  Lam'   :: TermXFamily ξ ν (TermX ξ ν) (Cons (NameScope (TermX ξ) ν) Nil)
  Let'   :: TermXFamily ξ ν (TermX ξ ν) (Cons (TermX ξ ν) (Cons (NameScope (TermX ξ) ν) Nil))
  Pi'    :: TermXFamily ξ ν (TermX ξ ν) (Cons (TypeX ξ ν) (Cons (NameScope (TypeX ξ) ν) Nil))
  Type'  :: TermXFamily ξ ν (TermX ξ ν) Nil
  Var'   :: TermXFamily ξ ν (TermX ξ ν) (Cons ν Nil)
  Scope' :: NameScope (TermX ξ) ν -> TermXFamily ξ ν (NameScope (TermX ξ) ν) Nil
  --Binder'   :: Binder   -> TermXFamily ξ ν Binder   Nil
  --Variable' :: Variable -> TermXFamily ξ ν Variable Nil

instance
  (ForallX Default ξ, ForallX Eq ξ, Eq ν, ForallX Show ξ, Show ν) =>
  Family (TermXFamily ξ ν) where

  decEq Annot' Annot' = Just (Refl, Refl)
  decEq   App'   App' = Just (Refl, Refl)
  decEq  Hole'  Hole' = Just (Refl, Refl)
  decEq   Lam'   Lam' = Just (Refl, Refl)
  decEq   Let'   Let' = Just (Refl, Refl)
  decEq    Pi'    Pi' = Just (Refl, Refl)
  decEq  Type'  Type' = Just (Refl, Refl)
  decEq   Var'   Var' = Just (Refl, Refl)
  decEq (Scope' s1) (Scope' s2)
    | s1 == s2 = Just (Refl, Refl)
    | otherwise = Nothing
  --decEq (Binder' b1) (Binder' b2)
  --  | b1 == b2 = Just (Refl, Refl)
  --  | otherwise = Nothing
  --decEq (Variable' v1) (Variable' v2)
  --  | v1 == v2 = Just (Refl, Refl)
  --  | otherwise = Nothing
  decEq _ _ = Nothing

  fields Annot' (Annot _ t τ)    = Just (CCons t (CCons τ CNil))
  fields   App' (App   _ t1 t2)  = Just (CCons t1 (CCons t2 CNil))
  fields  Hole' (Hole  _)        = Just CNil
  fields   Lam' (Lam   _ bt)     = Just (CCons bt CNil)
  fields   Let' (Let   _ t1 bt2) = Just (CCons t1 (CCons bt2 CNil))
  fields    Pi' (Pi    _ τ1 bτ2) = Just (CCons τ1 (CCons bτ2 CNil))
  fields  Type' (Type  _)        = Just CNil
  fields   Var' (Var     v)      = Just (CCons v CNil)
  --fields (Binder'   _) _ = Just CNil
  --fields (Variable' _) _ = Just CNil
  fields (Scope' _) _ = Just CNil
  fields _ _ = Nothing

  apply Annot' (CCons t (CCons τ CNil))    = Annot def t τ
  apply   App' (CCons t1 (CCons t2 CNil))  = App   def t1 t2
  apply  Hole' CNil                        = Hole  def
  apply   Lam' (CCons bt CNil)             = Lam   def bt
  apply   Let' (CCons t1 (CCons bt2 CNil)) = Let   def t1 bt2
  apply    Pi' (CCons τ1 (CCons bτ2 CNil)) = Pi    def τ1 bτ2
  apply  Type' CNil                        = Type  def
  apply   Var' (CCons v CNil)              = Var   v
  --apply (Binder'   b) CNil = b
  --apply (Variable' v) CNil = v
  --apply (Scope' s) (CCons t CNil) = s
  apply (Scope' s) CNil = s

  string  Annot' = "Annot"
  string    App' = "App"
  string   Hole' = "Hole"
  string    Lam' = "Lam"
  string    Let' = "Let"
  string     Pi' = "Pi"
  string   Type' = "Type"
  string    Var' = "Var"
  --string (Binder'   b) = show b
  --string (Variable' v) = show v
  string (Scope' s) = show s

--instance ForallX Default ξ => Type (TermXFamily ξ ν) Variable where
--    constructors = [ Abstr Variable' ]

--instance ForallX Default ξ => Type (TermXFamily ξ ν) Binder where
--    constructors = [ Abstr Binder' ]

instance
  ( ForallX Default ξ
  , ForallX Eq ξ
  , Eq ν
  , ForallX Show ξ
  , Show ν
  , Type (TermXFamily ξ ν) ν
  ) =>
  Type (TermXFamily ξ ν) (NameScope (TermX ξ) ν) where
  constructors = [ Abstr Scope' ]

instance
  ( ForallX Default ξ
  , ForallX Eq ξ
  , Eq ν
  , ForallX Show ξ
  , Show ν
  , Type (TermXFamily ξ ν) ν
  ) =>
  Type (TermXFamily ξ ν) (TermX ξ ν) where
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

{-
instance
  (ForallX Default ξ, Type (TermXFamily ξ ν) ν) =>
  Type (TermXFamily ξ ν) (TermX ξ (Var () (TermX ξ ν))) where
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
