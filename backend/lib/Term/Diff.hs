{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}

module Term.Diff
  ( TermDiff
  , TermXFamily(..)
  )where

import Bound.Name
-- import Bound.Scope
import Data.Default
import Data.Function
import Data.Generic.Diff

import Term.Binder
import Term.Variable
import Term.Term
import qualified Term.Universe as U

type TermDiff α = EditScript (TermXFamily α) (TermX α Variable) (TermX α Variable)

data TermXFamily α :: * -> * -> * where
  Annot' :: TermXFamily α (TermX α Variable)             (Cons (TermX α Variable)                  (Cons (TypeX α Variable)             Nil))
  App'   :: TermXFamily α (TermX α Variable)             (Cons (TermX α Variable)                  (Cons (TermX α Variable)             Nil))
  Hole'  :: TermXFamily α (TermX α Variable)                                                                                            Nil
  Lam'   :: TermXFamily α (TermX α Variable)             (Cons (ScopedTerm (TermX α) Variable)                                           Nil)
  Let'   :: TermXFamily α (TermX α Variable)             (Cons (TermX α Variable)                  (Cons (ScopedTerm (TermX α) Variable) Nil))
  Pi'    :: TermXFamily α (TermX α Variable)             (Cons (TypeX α Variable)                  (Cons (ScopedTerm (TypeX α) Variable) Nil))
  Type'  :: TermXFamily α (TermX α Variable)             (Cons U.Universe                                                               Nil)
  Var'   :: TermXFamily α (TermX α Variable)             (Cons Variable                                                                 Nil)
  Scope' :: TermXFamily α (ScopedTerm (TermX α) Variable) (Cons (Name Variable ())                  (Cons (TermX α Variable)             Nil))
  Name'     :: Name Variable () -> TermXFamily α (Name Variable ()) Nil
  Binder'   :: Binder Variable  -> TermXFamily α (Binder Variable)  Nil
  Variable' :: Variable         -> TermXFamily α Variable           Nil
  Universe' :: U.Universe       -> TermXFamily α U.Universe         Nil

instance
  (Default α, Eq α, Show α) =>
  Family (TermXFamily α) where

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
  fields  Type' (Type  u)        = Just (CCons u CNil)
  fields   Var' (Var   _ v)      = Just (CCons v CNil)
  fields Scope' s =
    let (b, t) = unscopeTerm s in
    let n = case unBinder b of
          Nothing -> error "fields Scope' binder" -- Variable "_"
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
  apply  Type' (CCons u CNil)              = Type  u
  apply   Var' (CCons v CNil)              = Var   def v
  apply Scope' (CCons n (CCons t CNil))    = abstractVariable (name n) t
  --apply (Scope' s) (CCons t CNil) = s
  apply (Name'     n) CNil = n
  apply (Binder'   b) CNil = b
  apply (Variable' v) CNil = v
  apply (Universe' u) CNil = u

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
  string (Universe' u) = show u

instance (Default α, Eq α, Show α) => Type (TermXFamily α) Variable where
  constructors = [ Abstr Variable' ]

instance (Default α, Eq α, Show α) => Type (TermXFamily α) (Binder Variable) where
  constructors = [ Abstr Binder' ]

instance (Default α, Eq α, Show α) => Type (TermXFamily α) (Name Variable ()) where
  constructors = [ Abstr Name' ]

instance (Default α, Eq α, Show α) => Type (TermXFamily α) U.Universe where
  constructors = [ Abstr Universe' ]

instance
  ( Default α
  , Eq α
  , Show α
  , Type (TermXFamily α) (Name Variable ())
  ) =>
  Type (TermXFamily α) (ScopedTerm (TermX α) Variable) where
  constructors = [ Concr Scope' ]

instance
  ( Default α
  , Eq α
  , Show α
  , Type (TermXFamily α) (Name Variable ())
  ) =>
  Type (TermXFamily α) (TermX α Variable) where
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
  (ForallX Default α, Type (TermXFamily α ν) ν) =>
  Type (TermXFamily α ν) (TermX α (Var () (TermX α ν))) where
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
