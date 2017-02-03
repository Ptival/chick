module Context where

import Term

type Context ξ = [(Name, TypeX ξ)]
