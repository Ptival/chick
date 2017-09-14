{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils
  ( foldlWith
  , foldrWith
  , isPi
  , mapWithIndex
  , orElse
  , orElse'
  , runSkipTrace
  , skipTrace
  , splitList
  , unzipMaybe
  , withState
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.State
import Control.Monad.Freer.Trace
import Control.Monad.Error.Class

import Term.Term

foldlWith :: Foldable t => (b -> a -> b) -> t a -> b -> b
foldlWith f l a = foldl f a l

foldrWith :: Foldable t => (a -> b -> b) -> t a -> b -> b
foldrWith f l a = foldr f a l

isPi :: TermX ξ ν -> Maybe (TermX ξ ν)
isPi t@(Pi _ _ _) = Just t
isPi _            = Nothing

mapWithIndex :: (a -> Int -> b) -> [a] -> [b]
mapWithIndex f l = map (\(e, i) -> f e i) (zip l [0..])

orElse :: MonadError e m => Maybe a -> e -> m a
orElse Nothing  e = throwError e
orElse (Just a) _ = return a

orElse' :: MonadError e m => Bool -> e -> m ()
orElse' False e = throwError e
orElse' True  _ = return ()

-- dumb, but has the same signature as `runTrace`, so easier to interchange
runSkipTrace :: Eff '[Trace] a -> IO a
runSkipTrace = return <$> skipTrace

skipTrace :: Eff '[Trace] a -> a
skipTrace (Val x) = x
skipTrace (E u q) =
  case extract u of
    Trace _ -> skipTrace (qApp q ())

splitList
   :: Int -> [a] -> Maybe ([a], a, [a])
splitList n xs =
  revL <$> go n xs
  where
    go 0 (h:t) = Just ([], h, t)
    go m (h:t) = prependL h <$> go (m-1) t
    go _ []    = Nothing
    prependL h (revl, x, r) = (h:revl, x, r)
    revL (l, x, r) = (reverse l, x, r)

unzipMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
unzipMaybe Nothing       = (Nothing, Nothing)
unzipMaybe (Just (a, b)) = (Just a,  Just b)

-- | `withState` localizes a modification of the state to a given effectful computation
withState ::
  Member (State s) r =>
  (s -> s) -> Eff r a -> Eff r a
withState f e = do
  s <- get
  put (f s)
  r <- e
  put s
  return r
