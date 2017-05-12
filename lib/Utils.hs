module Utils where

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
