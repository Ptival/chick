{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Typing.LocalContext where

import Control.Monad

import Text.PrettyPrint.Annotated.WL
import Typing.LocalDeclaration
import PrettyPrinting.PrettyPrintableAnnotated
import Term.Binder
import Term.Term
import Term.TypeChecked
import Term.Variable

newtype LocalContext ξ = LocalContext { unLocalContext :: [LocalDeclaration ξ] }
  deriving (Monoid)

instance PrettyPrintableAnnotated TermX =>
         PrettyPrintableAnnotated LocalContext where
  prettyDocA (LocalContext ctxt) = vsep <$> mapM prettyDocA (reverse ctxt)

addLocalAssum :: (Binder, TypeX ξ) -> LocalContext ξ -> LocalContext ξ
addLocalAssum (Binder Nothing , _) γ = γ
addLocalAssum (Binder (Just v), τ) (LocalContext γ) = LocalContext (LocalAssum v τ : γ)

type TypeCheckedLocalContext = LocalContext TypeChecked

lookupType :: Variable -> LocalContext ξ -> Maybe (TypeX ξ)
lookupType target (LocalContext γ) = msum (map found γ)
  where
    found = \case
      LocalAssum v τ   | v == target -> Just τ
      LocalDef   v _ τ | v == target -> Just τ
      _ -> Nothing
