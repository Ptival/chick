{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}

module Term.Diff where

import Bound.Name
import Bound.Scope
import Data.Default
import Data.Function
import Data.Generic.Diff

import Term.Binder
import Term.Variable
import Term.Term

type TermDiff ξ = EditScript (TermXFamily ξ) (TermX ξ Variable) (TermX ξ Variable)

data TermXFamily ξ :: * -> * -> * where
  Annot' :: TermXFamily ξ (TermX ξ Variable)             (Cons (TermX ξ Variable)                  (Cons (TypeX ξ Variable)             Nil))
  App'   :: TermXFamily ξ (TermX ξ Variable)             (Cons (TermX ξ Variable)                  (Cons (TermX ξ Variable)             Nil))
  Hole'  :: TermXFamily ξ (TermX ξ Variable)                                                                                            Nil
  Lam'   :: TermXFamily ξ (TermX ξ Variable)             (Cons (NameScope (TermX ξ) Variable)                                           Nil)
  Let'   :: TermXFamily ξ (TermX ξ Variable)             (Cons (TermX ξ Variable)                  (Cons (NameScope (TermX ξ) Variable) Nil))
  Pi'    :: TermXFamily ξ (TermX ξ Variable)             (Cons (TypeX ξ Variable)                  (Cons (NameScope (TypeX ξ) Variable) Nil))
  Type'  :: TermXFamily ξ (TermX ξ Variable)                                                                                            Nil
  Var'   :: TermXFamily ξ (TermX ξ Variable)             (Cons Variable                                                                 Nil)
  Scope' :: TermXFamily ξ (NameScope (TermX ξ) Variable) (Cons (Name Variable ())                   (Cons (TermX ξ Variable)             Nil))
  Name'  :: Name Variable () -> TermXFamily ξ (Name Variable ()) Nil
  Binder'   :: Binder Variable -> TermXFamily ξ (Binder Variable) Nil
  Variable' :: Variable -> TermXFamily ξ Variable Nil

instance
  (Default ξ, Eq ξ, Show ξ) =>
  Family (TermXFamily ξ) where

  decEq Annot' Annot' = Just (Refl, Refl)
  decEq   App'   App' = Just (Refl, Refl)
  decEq  Hole'  Hole' = Just (Refl, Refl)
  decEq   Lam'   Lam' = Just (Refl, Refl)
  decEq   Let'   Let' = Just (Refl, Refl)
  decEq    Pi'    Pi' = Just (Refl, Refl)
  decEq  Type'  Type' = Just (Refl, Refl)
  decEq   Var'   Var' = Just (Refl, Refl)
  decEq Scope' Scope' = Just (Refl, Refl)
  decEq (Name' n1) (Name' n2)
    | on (==) name n1 n2 = Just (Refl, Refl)
    | otherwise         = Nothing
  decEq (Binder' b1) (Binder' b2)
    | b1 == b2 = Just (Refl, Refl)
    | otherwise = Nothing
  decEq (Variable' v1) (Variable' v2)
    | v1 == v2 = Just (Refl, Refl)
    | otherwise = Nothing
  decEq _ _ = Nothing

  fields Annot' (Annot _ t τ)    = Just (CCons t (CCons τ CNil))
  fields   App' (App   _ t1 t2)  = Just (CCons t1 (CCons t2 CNil))
  fields  Hole' (Hole  _)        = Just CNil
  fields   Lam' (Lam   _ bt)     = Just (CCons bt CNil)
  fields   Let' (Let   _ t1 bt2) = Just (CCons t1 (CCons bt2 CNil))
  fields    Pi' (Pi    _ τ1 bτ2) = Just (CCons τ1 (CCons bτ2 CNil))
  fields  Type' (Type)           = Just CNil
  fields   Var' (Var     v)      = Just (CCons v CNil)
  fields Scope' s =
    let (b, t) = unscopeTerm s in
    let n = case unBinder b of
          Nothing -> Variable "_"
          Just v  -> v
    in
    Just (CCons (Name n ()) (CCons t CNil))
  fields (Name'     _) _ = Just CNil
  fields (Binder'   _) _ = Just CNil
  fields (Variable' _) _ = Just CNil
  fields _ _ = Nothing

  apply Annot' (CCons t (CCons τ CNil))    = Annot def t τ
  apply   App' (CCons t1 (CCons t2 CNil))  = App   def t1 t2
  apply  Hole' CNil                        = Hole  def
  apply   Lam' (CCons bt CNil)             = Lam   def bt
  apply   Let' (CCons t1 (CCons bt2 CNil)) = Let   def t1 bt2
  apply    Pi' (CCons τ1 (CCons bτ2 CNil)) = Pi    def τ1 bτ2
  apply  Type' CNil                        = Type
  apply   Var' (CCons v CNil)              = Var   v
  apply Scope' (CCons n (CCons t CNil))    = abstract1Name (name n) t
  --apply (Scope' s) (CCons t CNil) = s
  apply (Name'     n) CNil = n
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
  string  Scope' = "Scope"
  string (Name'     n) = show n
  string (Binder'   b) = show b
  string (Variable' v) = show v

instance (Default ξ, Eq ξ, Show ξ) => Type (TermXFamily ξ) Variable where
  constructors = [ Abstr Variable' ]

instance (Default ξ, Eq ξ, Show ξ) => Type (TermXFamily ξ) (Binder Variable) where
  constructors = [ Abstr Binder' ]

instance (Default ξ, Eq ξ, Show ξ) => Type (TermXFamily ξ) (Name Variable ()) where
  constructors = [ Abstr Name' ]

instance
  ( Default ξ
  , Eq ξ
  , Show ξ
  , Type (TermXFamily ξ) (Name Variable ())
  ) =>
  Type (TermXFamily ξ) (NameScope (TermX ξ) Variable) where
  constructors = [ Concr Scope' ]

instance
  ( Default ξ
  , Eq ξ
  , Show ξ
  , Type (TermXFamily ξ) (Name Variable ())
  ) =>
  Type (TermXFamily ξ) (TermX ξ Variable) where
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
