module Utils
  ( isPi
  , orElse
  , orElse'
  , splitList
  ) where

import Control.Monad.Error.Class

import Term.Term

orElse :: MonadError e m => Maybe a -> e -> m a
orElse Nothing  e = throwError e
orElse (Just a) _ = return a

orElse' :: MonadError e m => Bool -> e -> m ()
orElse' False e = throwError e
orElse' True  _ = return ()

isPi :: TermX ξ ν -> Maybe (TermX ξ ν)
isPi t@(Pi _ _ _) = Just t
isPi _            = Nothing

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
