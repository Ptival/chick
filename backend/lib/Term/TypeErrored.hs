-- {-# language FlexibleInstances #-}
-- {-# language MultiParamTypeClasses #-}
-- {-# language TypeSynonymInstances #-}
-- {-# language UndecidableInstances #-}

module Term.TypeErrored
  ( Annotation
  , Term
  , Type
  , TypeError
  , annotateError
  ) where

import Data.Bifunctor

import qualified Term.Raw            as R
import           Term.Term
import qualified Term.TypeChecked    as C
import           TypeCheckingFailure

type TypeError = TypeCheckingFailure R.Term

type Annotation ν = (Either (TypeError ν) (C.Checked ν))
type Term ν = TermX (Annotation ν) ν
type Type ν = Term ν

{-
`annotateError e term` marks the entire `term` unchecked, and marks its head
with the error `e`
-}
annotateError :: TypeError ν -> TermX α ν -> Term ν
annotateError e t = annotateHead (Left e) (first (const (Left Unchecked)) t)

-- getTypeError :: Term ν -> Maybe (Either (TypeError ν) (C.Checked ν))
-- getTypeError = annotationOf
