{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Bound.ScopeT where

import Bound
import Bound.Name
import Control.Monad (liftM)
import Control.Comonad
import Data.Functor.Classes

-- |
-- @
-- 'Scope' n f a ~ 'ScopeT' n 'Identity' f a
-- 'ScopeT' n t f a ~ t ('Scope' n f a)
-- @
newtype ScopeT n t f a = ScopeT { unscopeT :: t f (Var n (f a)) }
  deriving (Functor, Foldable, Traversable)

infixl 1 >>>>=
(>>>>=) :: (Monad f, Functor (t f)) => ScopeT n t f a -> (a -> f b) -> ScopeT n t f b
ScopeT m >>>>= k = ScopeT $ fmap (fmap (>>= k)) m

-------------------------------------------------------------------------------
-- Abstraction
-------------------------------------------------------------------------------

abstractT :: (Functor (t f), Monad f) => (a -> Maybe n) -> t f a -> ScopeT n t f a
abstractT f e = ScopeT (fmap k e) where
    k y = case f y of
        Just z  -> B z
        Nothing -> F (return y)
{-# INLINE abstractT #-}

-- | Abstract over a single variable.
--
-- >>> abstract1T 'x' (MaybeT (Nothing : map Just "xyz"))
-- ScopeT (MaybeT [Nothing,Just (B ()),Just (F "y"),Just (F "z")])
abstract1T :: (Functor (t f), Monad f, Eq a) => a -> t f a -> ScopeT () t f a
abstract1T a = abstractT (\b -> if a == b then Just () else Nothing)
{-# INLINE abstract1T #-}

-- TODO: abstractEither

-- | Abstraction, capturing named bound variables.
abstractTName :: (Functor (t f), Monad f) => (a -> Maybe b) -> t f a -> ScopeT (Name a b) t f a
abstractTName f t = ScopeT (fmap k t) where
    k a = case f a of
        Just b  -> B (Name a b)
        Nothing -> F (return a)
{-# INLINE abstractTName #-}

-- | Abstract over a single variable
abstract1TName :: (Functor (t f), Monad f, Eq a) => a -> t f a -> ScopeT (Name a ()) t f a
abstract1TName a = abstractTName (\b -> if a == b then Just () else Nothing)
{-# INLINE abstract1TName #-}

-------------------------------------------------------------------------------
-- Instantiation
-------------------------------------------------------------------------------

instantiateT :: (Bound t, Monad f) => (n -> f a) -> ScopeT n t f a -> t f a
instantiateT k (ScopeT e) = e >>>= \v -> case v of
    B b -> k b
    F a -> a

instantiate1T :: (Bound t, Monad f) => f a -> ScopeT n t f a -> t f a
instantiate1T e = instantiateT (const e)

-- TODO: instantiateEitherT

-------------------------------------------------------------------------------
-- Traditional de Bruijn
-------------------------------------------------------------------------------

fromScopeT :: (Bound t, Monad f) => ScopeT n t f a -> t f (Var n a)
fromScopeT (ScopeT s) = s >>>= \v -> case v of
    F e -> fmap F e
    B b -> return (B b)

toScopeT :: (Functor (t f), Monad f) => t f (Var n a) -> ScopeT n t f a
toScopeT e = ScopeT (fmap (fmap return) e)

lowerScopeT
    :: Functor (t f)
    => (forall x. t f x -> g x)
    -> (forall x. f x -> g x)
    -> ScopeT n t f a -> Scope n g a
lowerScopeT tf f (ScopeT x) = Scope (tf (fmap (fmap f) x))

{-
wrapScope :: (forall x. f x -> t f x) -> Scope n f a -> ScopeT n t f a
wrapScope f (Scope x) = ScopeT (f x)
-}

-------------------------------------------------------------------------------
-- Extras
-------------------------------------------------------------------------------

-- | Return a list of occurences of the variables bound by this 'Scope'.
bindingsT :: Foldable (t f) => ScopeT b t f a -> [b]
bindingsT (ScopeT s) = foldr f [] s where
  f (B v) vs = v : vs
  f _ vs     = vs
{-# INLINE bindingsT #-}

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------

instance (Show n, Show1 (t f), Show1 f) => Show1 (ScopeT n t f) where
    liftShowsPrec sp sl d (ScopeT x) = showsUnaryWith
        (liftShowsPrec (liftShowsPrec2 showsPrec undefined (liftShowsPrec sp sl) undefined) undefined)
        "ScopeT" d x

instance (Show n, Show1 (t f), Show1 f, Show a) => Show (ScopeT n t f a) where
    showsPrec = showsPrec1

-- $setup
-- >>> import Control.Monad.Trans.Maybe

instance (Bound t, Eq n, Eq1 (t f), Monad f) => Eq1 (ScopeT n t f) where
  liftEq eqVar m n = liftEq (liftEq eqVar) (fromScopeT m) (fromScopeT n)

instance (Bound t, Eq a, Eq n, Eq1 (t f), Monad f) => Eq (ScopeT n t f a) where
  (==) = eq1

hoistScopeT :: (Functor (t f)) =>
  (forall x. t f x -> t g x) ->
  (forall x. f x -> g x) ->
  ScopeT b t f a -> ScopeT b t g a
hoistScopeT t f (ScopeT b) = ScopeT $ t $ (f <$>) <$> b

instantiateNameT ::
  (Bound t, Monad f, Comonad n) =>
  (b -> f a) -> ScopeT (n b) t f a -> t f a
instantiateNameT k e = unscopeT e >>>= \v -> case v of
  B b -> k (extract b)
  F a -> a

abstractNameT ::
  (Applicative f, Bound t, Functor (t f)) =>
  (a -> Maybe b) -> t f a -> ScopeT (Name a b) t f a
abstractNameT f t = ScopeT $ k <$> t
  where
    k a = case f a of
      Just b  -> B (Name a b)
      Nothing -> F (pure a)
{-# INLINE abstractNameT #-}
