module Inductive.Utils
  ( foldlWith
  , foldrWith
  , instantiateBinders
  ) where

import Text.Printf

import Term.Term

foldlWith :: Foldable t => (b -> a -> b) -> t a -> b -> b
foldlWith f l a = foldl f a l

foldrWith :: Foldable t => (a -> b -> b) -> t a -> b -> b
foldrWith f l a = foldr f a l

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = map (\(e, i) -> f e i) (zip l [0..])

instantiateBinders ::
  String ->
  [(Binder Variable, TypeX α Variable)] ->
  [(Variable, TypeX α Variable)]
instantiateBinders prefix = mapWithIndex instantiate
  where
    instantiate (Binder (Just v ), τ) _ = (v,      τ)
    instantiate (Binder Nothing,   τ) i = (Variable $ printf "%s%d" prefix i, τ)
