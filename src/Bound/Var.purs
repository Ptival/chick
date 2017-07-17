module Bound.Var where

import Prelude
import Data.Array (fold)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bitraversable)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Generic (class Generic)
import Data.Monoid (mempty)
import Data.Traversable (class Traversable)

data Var b a
  = B b -- | this is a bound variable
  | F a -- | this is a free variable

derive instance genericVar :: (Generic b, Generic a) => Generic (Var b a)

instance eqVar :: (Eq b, Eq a) => Eq (Var b a) where
  eq (B b1) (B b2) = b1 == b2
  eq (F a1) (F a2) = a1 == a1
  eq _      _      = false

instance eq1Var :: Eq b => Eq1 (Var b) where
  eq1 = eq

instance functorVar :: Functor (Var b) where
  map _ (B b) = B b
  map f (F a) = F (f a)

instance foldableVar :: Foldable (Var b) where
  foldMap f (F a) = f a
  foldMap _ _     = mempty
  foldl f acc (F a) = f acc a
  foldl _ acc _     = acc
  foldr f acc (F a) = f a acc
  foldr _ acc _     = acc

instance traversableVar :: Traversable (Var b) where
  traverse f (F a) = F <$> f a
  traverse _ (B b) = pure (B b)
  sequence (F a) = F <$> a
  sequence (B b) = pure (B b)

instance applyVar :: Apply (Var b) where
  apply (F f) (F a) = F (f a)
  apply (B b) _     = B b -- !?
  apply _     (B b) = B b

instance applicativeVar :: Applicative (Var b) where
  pure = F

instance bindVar :: Bind (Var b) where
  bind (F a) f = f a
  bind (B b) _ = B b

instance monadVar :: Monad (Var b) where

instance bifunctorVar :: Bifunctor Var where
  bimap f _ (B b) = B (f b)
  bimap _ g (F a) = F (g a)

instance bifoldableVar :: Bifoldable Var where
  bifoldMap f _ (B b) = f b
  bifoldMap _ g (F a) = g a
  bifoldl fb fa acc (B b) = fb acc b
  bifoldl fb fa acc (F a) = fa acc a
  bifoldr fb fa acc (B b) = fb b acc
  bifoldr fb fa acc (F a) = fa a acc

instance bitraversableVar :: Bitraversable Var where
  bitraverse f _ (B b) = B <$> f b
  bitraverse _ g (F a) = F <$> g a
  bisequence (B b) = B <$> b
  bisequence (F a) = F <$> a

instance showVar :: (Show b, Show a) => Show (Var b a) where
  show (B b) = fold ["(B (", show b, "))"]
  show (F a) = fold ["(F (", show a, "))"]
