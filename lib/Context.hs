module Context where

import Term

type Context ξ = [(Variable, TypeX ξ)]

(+:) :: (Binder, TypeX ξ) -> Context ξ -> Context ξ
(Binder Nothing , _) +: γ = γ
(Binder (Just n), τ) +: γ = (n, τ) : γ
