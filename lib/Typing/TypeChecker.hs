module Typing.TypeChecker where

import Term.Term
import Term.TypeChecked         as TypeChecked
import Term.TypeErrored         as TypeErrored
import Typing.GlobalEnvironment
import Work

typeCheck ::
  GlobalEnvironment (Checked Variable) Variable ->
  TermX ξ Variable -> TermX ψ Variable ->
  Either (TypeErrored.Term Variable) (TypeChecked.Term Variable)
typeCheck ge t τ = tc (checkF (toLocalContext ge) t τ id)

typeSynth ::
  GlobalEnvironment (Checked Variable) Variable -> TermX ξ Variable ->
  Either (TypeErrored.Term Variable) (TypeChecked.Term Variable)
typeSynth ge t = tc (synthF (toLocalContext ge) t id)
