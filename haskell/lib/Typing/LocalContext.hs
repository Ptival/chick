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
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Binder
import Term.Term
import Term.TypeChecked
--import Term.Variable
--import Term.Raw as Raw

newtype LocalContext ξ ν =
  LocalContext { unLocalContext :: [LocalDeclaration ξ ν] }
  deriving (Eq, Monoid, Show)

instance PrettyPrintableUnannotated (TermX ξ) =>
         PrettyPrintableUnannotated (LocalContext ξ) where
  prettyDocU (LocalContext ctxt) = vsep <$> mapM prettyDocU (reverse ctxt)

addLocalAssum :: (Binder ν, TypeX ξ ν) -> LocalContext ξ ν -> LocalContext ξ ν
addLocalAssum (Binder Nothing , _) γ = γ
addLocalAssum (Binder (Just v), τ) (LocalContext γ) = LocalContext (LocalAssum v τ : γ)

type TypeCheckedLocalContext ν = LocalContext (Checked ν) ν

lookupType :: Eq ν => ν -> LocalContext ξ ν -> Maybe (TypeX ξ ν)
lookupType target (LocalContext γ) = msum (map found γ)
  where
    found = \case
      LocalAssum v τ   | v == target -> Just τ
      LocalDef   v _ τ | v == target -> Just τ
      _ -> Nothing
