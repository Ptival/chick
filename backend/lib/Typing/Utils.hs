{-# LANGUAGE FlexibleContexts #-}

module Typing.Utils
  ( (|||)
  , (^||)
  , (?||)
  , (!)
  , (~!)
  , (~!\)
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Data.Bifunctor

import           Term.Term
import qualified Term.TypeChecked as C
import qualified Term.TypeErrored as E
import           TypeCheckingFailure

type Error   = E.Term Variable

(|||) :: Member (Exc Error) r => Eff r a -> (Error -> Error) -> Eff r a
(|||) e f = do
  e `catchError` (throwError . f)

(^||) :: Member (Exc Error) r => Maybe a -> Error -> Eff r a
(^||) m e = maybe (throwError e) pure m

(?||) :: Member (Exc Error) r => Bool -> Error -> Eff r ()
(?||) b e = if b then return () else throwError e

-- | Stands for "checked"
(!) :: C.Term ν -> E.Term ν
(!) = first Right

-- | Stands for "not checked"
(~!) :: TermX α ν -> E.Term ν
(~!) = first (const (Left Unchecked))

-- | Stands for "not checked lambda"
(~!\) :: ScopedTerm (TermX ν) Variable -> ScopedTerm (TermX (E.Annotation Variable)) Variable
(~!\) s =
  let (b, t) = unscopeTerm s in
  abstractBinder b ((~!) t)
