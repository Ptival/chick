{-# language FlexibleContexts #-}

module Notations where

import Data.Default

import Context
import Term.Term

-- Annot
infix 0 ^::
(^::) :: ForallX Default ξ => TypeX ξ -> TermX ξ -> TermX ξ
t ^:: τ = Annot def t τ

-- App
infixl 9 ^$
(^$) :: ForallX Default ξ => TermX ξ -> TermX ξ -> TermX ξ
t1 ^$ t2 = App def t1 t2

-- Hole
hole :: ForallX Default ξ => TermX ξ
hole = Hole def

-- Lam
(^\) :: ForallX Default ξ => [Variable] -> TermX ξ -> TermX ξ
(^\) []       t = t
(^\) (n : ns) t = Lam def (Binder (Just n)) ((^\) ns t)

-- Let
let' :: ForallX Default ξ => [(Variable, TermX ξ)] -> TermX ξ -> TermX ξ
let' []             t  = t
let' ((n, t1) : ns) t2 = Let def (Binder (Just n)) t1 (let' ns t2)

-- Pi (named)
π :: ForallX Default ξ => Context ξ -> TermX ξ -> TermX ξ
π []             t = t
π ((n, τ) : nτs) t = Pi def (Binder (Just n)) τ (π nτs t)

-- Pi (anonymous)
infixr 1 ^->
(^->) :: ForallX Default ξ => TypeX ξ -> TermX ξ -> TermX ξ
τ ^-> t = Pi def (Binder Nothing) τ t

-- Type
type' :: ForallX Default ξ => TypeX ξ
type' = Type def

-- Var
var :: ForallX Default ξ => Variable -> TermX ξ
var = Var def
