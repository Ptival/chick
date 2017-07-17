module Bound.Scope where

import Prelude
import Bound.Bound (class Bound)
import Bound.Var (Var(..))
import Data.Array (fold, foldr, (:))
import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Traversable (class Traversable, sequenceDefault, traverse)

newtype Scope b f a = Scope (f (Var b (f a)))

instance showScope :: Show (f (Var b (f a))) => Show (Scope b f a) where
  show (Scope s) = fold ["Scope (", show s, ")"]

unscope :: ∀ b f a. Scope b f a -> f (Var b (f a))
unscope (Scope f) = f

fromScope :: ∀ f b a. Applicative f => Bind f => Scope b f a -> f (Var b a)
fromScope (Scope s) = s >>= \v -> case v of
  F e -> liftA1 F e
  B b -> pure (B b)

instance functorScope :: Functor f => Functor (Scope b f) where
  map f (Scope a) = Scope (map (map (map f)) a)

instance foldableScope :: Foldable f => Foldable (Scope b f) where
  foldMap f (Scope a) = foldMap (foldMap (foldMap f)) a
  foldl t = foldlDefault t
  foldr t = foldrDefault t

instance traversableScope :: Traversable f => Traversable (Scope b f) where
  traverse f (Scope a) = Scope <$> traverse (traverse (traverse f)) a
  sequence = sequenceDefault

-- | I have no idea whether this is correct, but it type-checks...
instance applyScope :: (Applicative f, Apply f, Bind f) => Apply (Scope b f) where
  apply (Scope f) (Scope e) = Scope $ e >>= case _ of
    B b -> pure (B b)
    F ea -> f >>= pure <$> case _ of
      B b -> B b
      F a -> F $ apply a ea

instance applicativeScope :: (Applicative f, Bind f) => Applicative (Scope b f) where
  pure a = Scope (pure (F (pure a)))

instance bindScope :: (Applicative f, Bind f) => Bind (Scope b f) where
  bind (Scope e) f = Scope $ e >>= case _ of
    B b  -> pure (B b)
    F ea -> ea >>= unscope <<< f

instance eq1Scope :: (Eq b, Eq1 f, Monad f) => Eq1 (Scope b f) where
  eq1 m n = eq1 (fromScope m) (fromScope n)

instance eqScope :: (Eq b, Eq1 f, Eq a, Monad f) => Eq (Scope b f a) where
  eq = eq1

instance boundScope :: Bound (Scope b) where
  boundSubst (Scope m) f = Scope (liftA1 (map (_ >>= f)) m)

hoistScope :: ∀ f g b a. Functor f => (∀ x. f x -> g x) -> Scope b f a -> Scope b g a
hoistScope t (Scope b) = Scope $ t (map t <$> b)

bindings :: ∀ b f a. Foldable f => Scope b f a -> Array b
bindings (Scope s) = foldr f [] s
  where
    f (B v) vs = v : vs
    f _     vs = vs

instantiate :: ∀ b f a. Bind f => (b -> f a) -> Scope b f a -> f a
instantiate k e = unscope e >>= \v -> case v of
  B b -> k b
  F a -> a

instantiate1 :: ∀ n f a. Bind f => f a -> Scope n f a -> f a
instantiate1 e = instantiate (const e)
