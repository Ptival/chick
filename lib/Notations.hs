{-# language FlexibleContexts #-}

module Notations where

--import Bound
import Bound.Name
import Data.Default

import Term.Term
import Term.Variable

-- Annot
infix 0 ^::
(^::) :: ForallX Default ξ => TypeX ξ ν -> TermX ξ ν -> TermX ξ ν
t ^:: τ = Annot def t τ

-- App
infixl 9 ^$
(^$) :: ForallX Default ξ => TermX ξ ν -> TermX ξ ν -> TermX ξ ν
t1 ^$ t2 = App def t1 t2

-- Hole
hole :: ForallX Default ξ => TermX ξ ν
hole = Hole def

-- Lam
(^\) :: (ForallX Default ξ) => [Variable] -> TermX ξ Variable -> TermX ξ Variable
(^\) []       t = t
(^\) (n : ns) t = Lam def (abstract1Name n ((^\) ns t))

-- Let
let' :: (ForallX Default ξ) => [(Variable, TermX ξ Variable)] -> TermX ξ Variable -> TermX ξ Variable
let' []             t  = t
let' ((n, t1) : ns) t2 = Let def t1 (abstract1Name n (let' ns t2))

-- Pi (named)
π :: (ForallX Default ξ) => [(Variable, TypeX ξ Variable)] -> TermX ξ Variable -> TermX ξ Variable
π []             t = t
π ((n, τ) : nτs) t = Pi def τ (abstract1Name n (π nτs t))

-- Pi (anonymous)
infixr 1 ^->
(^->) :: ForallX Default ξ => TypeX ξ Variable -> TermX ξ Variable -> TermX ξ Variable
τ ^-> t = Pi def τ (abstractAnonymous t)

-- Type
type' :: ForallX Default ξ => TypeX ξ ν
type' = Type def

-- Var
var :: ForallX Default ξ => ν -> TermX ξ ν
var = Var
