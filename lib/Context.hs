module Context where

import Term.Term
import Term.TypeChecked

type Context ξ = [(Variable, TypeX ξ)]

(+:) :: (Binder, TypeX ξ) -> Context ξ -> Context ξ
(Binder Nothing , _) +: γ = γ
(Binder (Just n), τ) +: γ = (n, τ) : γ

type TypeCheckedContext = Context TypeChecked
