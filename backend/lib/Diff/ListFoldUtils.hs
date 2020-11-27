module Diff.ListFoldUtils
  ( δapplyBinders,
    δapplyTerms,
    δapplyVariables,
    δquantifyBinders,
    δquantifyVariables,
  )
where

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import Diff.ListFoldLeft
  ( δListFoldLeft,
    δListFoldMkAppBinders,
    δListFoldMkAppTerms,
    δListFoldMkAppVariables,
    δListFoldMkPiBinders,
    δListFoldMkPiVariables,
  )
import Diff.ListFoldRight
  ( δListFoldRight,
  )
import qualified Diff.Pair as D2
import qualified Diff.Term as DT
import qualified Diff.Triple as D3
import Term.Term (Binder, TermX, Variable)

δquantifyVariables ::
  -- PrettyPrintable α =>
  [(α, Variable, TermX α Variable)] ->
  DL.Diff
    (α, Variable, TermX α Variable)
    (D3.Diff (DA.Diff α) (DA.Diff Variable) (DT.Diff α)) ->
  DT.Diff α ->
  DT.Diff α
δquantifyVariables = δListFoldRight δListFoldMkPiVariables

δquantifyBinders ::
  -- PrettyPrintable α =>
  [(α, Binder Variable, TermX α Variable)] ->
  DL.Diff
    (α, Binder Variable, TermX α Variable)
    (D3.Diff (DA.Diff α) (DA.Diff (Binder Variable)) (DT.Diff α)) ->
  DT.Diff α ->
  DT.Diff α
δquantifyBinders = δListFoldRight δListFoldMkPiBinders

δapplyTerms ::
  [(α, TermX α Variable)] ->
  DL.Diff (α, TermX α Variable) (D2.Diff (DA.Diff α) (DT.Diff α)) ->
  DT.Diff α ->
  DT.Diff α
δapplyTerms = δListFoldLeft δListFoldMkAppTerms

δapplyVariables ::
  -- PrettyPrintable α =>
  [(α, Variable, TermX α Variable)] ->
  DL.Diff
    (α, Variable, TermX α Variable)
    (D3.Diff (DA.Diff α) (DA.Diff Variable) (DT.Diff α)) ->
  DT.Diff α ->
  DT.Diff α
δapplyVariables = δListFoldLeft δListFoldMkAppVariables

δapplyBinders ::
  -- PrettyPrintable α =>
  [(α, Binder Variable, TermX α Variable)] ->
  DL.Diff
    (α, Binder Variable, TermX α Variable)
    (D3.Diff (DA.Diff α) (DA.Diff (Binder Variable)) (DT.Diff α)) ->
  DT.Diff α ->
  DT.Diff α
δapplyBinders = δListFoldLeft δListFoldMkAppBinders
