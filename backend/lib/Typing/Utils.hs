module Typing.Utils
  ( (|||)
  , (^||)
  , (?||)
  , (!)
  , (~!)
  , (~!\)
  ) where

import           Data.Bifunctor
import           Polysemy            ( Member, Sem )
import           Polysemy.Error      ( Error, catch, throw )

import           Term.Term
import qualified Term.TypeChecked    as C
import qualified Term.TypeErrored    as E
import           TypeCheckingFailure

type ErrorTerm   = E.Term Variable

(|||) :: Member (Error ErrorTerm) r => Sem r a -> (ErrorTerm -> ErrorTerm) -> Sem r a
(|||) e f = do
  e `catch` (throw . f)

(^||) :: Member (Error ErrorTerm) r => Maybe a -> ErrorTerm -> Sem r a
(^||) m e = maybe (throw e) pure m

(?||) :: Member (Error ErrorTerm) r => Bool -> ErrorTerm -> Sem r ()
(?||) b e = if b then return () else throw e

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
