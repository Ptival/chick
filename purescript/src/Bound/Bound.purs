module Bound.Bound where

import Prelude

class Bound t where
  boundSubst :: ∀ f a c. Applicative f => Bind f => t f a -> (a -> f c) -> t f c

infixl 1 boundSubst as >>>=
