{-# language FlexibleContexts #-}

module Notations where

--import Bound
import           Bound.Name
import           Data.Default

import           Term.Term
import qualified Term.Universe as U

-- Annot
infix 0 ^::
(^::) :: Default α => TypeX α ν -> TermX α ν -> TermX α ν
t ^:: τ = Annot def t τ

-- App
infixl 9 ^$
(^$) :: Default α => TermX α ν -> TermX α ν -> TermX α ν
t1 ^$ t2 = App def t1 t2

-- Hole
hole :: Default α => TermX α ν
hole = Hole def

-- Lam
(^\) :: (Default α) => [Variable] -> TermX α Variable -> TermX α Variable
(^\) []       t = t
(^\) (n : ns) t = Lam def (abstract1Name n ((^\) ns t))

-- Let
let' :: (Default α) => [(Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
let' []             t  = t
let' ((n, t1) : ns) t2 = Let def t1 (abstract1Name n (let' ns t2))

-- Pi (named)
π :: (Default α) => [(Variable, TypeX α Variable)] -> TermX α Variable -> TermX α Variable
π []             t = t
π ((n, τ) : nτs) t = Pi def τ (abstract1Name n (π nτs t))

-- Pi (anonymous)
infixr 1 ^->
(^->) :: Default α => TypeX α Variable -> TermX α Variable -> TermX α Variable
τ ^-> t = Pi def τ (abstractAnonymous t)

-- Type
prop :: Default α => TypeX α ν
prop = Type U.Prop

set :: Default α => TypeX α ν
set = Type U.Set

type' :: Default α => TypeX α ν
type' = Type U.Type

-- Var
var :: Default α => ν -> TermX α ν
var = Var (Just def)
