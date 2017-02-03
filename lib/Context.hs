module Context where

import Term

type Context ξ = [(Name, TypeX ξ)]

(+:) :: (Maybe Name, TypeX ξ) -> Context ξ -> Context ξ
(Nothing, _) +: γ = γ
(Just n,  τ) +: γ = (n, τ) : γ
