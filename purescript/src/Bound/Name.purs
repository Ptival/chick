module Bound.Name where

import Prelude
import Bound.Scope (Scope(..), instantiate1)
import Bound.Var (Var(..))
import Data.Array (fold)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))

data Name n b = Name n b

derive instance genericName :: (Generic n, Generic b) => Generic (Name n b)

instance showName :: (Show n, Show b) => Show (Name n b) where
  show (Name n b) = fold ["Name (", show n, ") (", show b, ")"]

abstractName :: ∀ f a b. Monad f => (a -> Maybe b) -> f a -> Scope (Name a b) f a
abstractName f t = Scope (liftM1 k t) where
  k a = case f a of
    Just b  -> B (Name a b)
    Nothing -> F (pure a)

abstract1Name :: ∀ f a. Eq a => Monad f => a -> f a -> Scope (Name a Unit) f a
abstract1Name a = abstractName (\b -> if a == b then Just unit else Nothing)

instantiate1Name :: ∀ n f a. Monad f => f a -> Scope n f a -> f a
instantiate1Name = instantiate1
