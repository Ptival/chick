module Typing.TypeChecker where

import Term.Term
import Term.TypeChecked         as TypeChecked
import Term.TypeErrored         as TypeErrored
import Typing.GlobalEnvironment
import Work

typeCheck ::
  GlobalEnvironment TypeChecked -> TermX ξ -> TermX ψ ->
  Either TypeErrored.Term TypeChecked.Term
typeCheck ge t τ = tc (checkF (toLocalContext ge) t τ id)

typeSynth ::
  GlobalEnvironment TypeChecked -> TermX ξ ->
  Either TypeErrored.Term TypeChecked.Term
typeSynth ge t = tc (synthF (toLocalContext ge) t id)
