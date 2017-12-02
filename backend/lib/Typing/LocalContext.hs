{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Typing.LocalContext
  ( LocalContext(..)
  , TypeCheckedLocalContext
  , addHyp
  , addLocalAssum
  , boundNames
  , lookupType
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Default
import Data.Maybe
import Text.PrettyPrint.Annotated.WL

import Typing.LocalDeclaration
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Binder
import Term.Term
import Term.TypeChecked

newtype LocalContext α ν =
  LocalContext { unLocalContext :: [LocalDeclaration α ν] }
  deriving (Eq, Monoid, Show)

instance
  PrettyPrintableUnannotated (TermX α Variable) =>
  PrettyPrintableUnannotated (LocalContext α Variable) where
  prettyDocU (LocalContext ctxt) = encloseSep lbracket rbracket comma <$> mapM prettyDocU (reverse ctxt)

instance
  PrettyPrintableUnannotated (TermX α Variable) =>
  PrettyPrintable (LocalContext α Variable) where
  prettyDoc c = runReader (prettyDocU c) def

addHyp ::
  (Eq ν, MonadError String m) =>
  LocalDeclaration α ν -> LocalContext α ν -> m (LocalContext α ν)
addHyp hyp (LocalContext hyps)
  | nameOf hyp `elem` map nameOf hyps = throwError "addHyp: name conflict"
  | otherwise = return . LocalContext $ hyp:hyps

addLocalAssum :: (Binder ν, TypeX α ν) -> LocalContext α ν -> LocalContext α ν
addLocalAssum (b, τ) (LocalContext γ) = LocalContext (LocalAssum b τ : γ)

type TypeCheckedLocalContext ν = LocalContext (Checked ν) ν

lookupType :: Eq ν => ν -> LocalContext α ν -> Maybe (TypeX α ν)
lookupType target (LocalContext γ) = msum (map found γ)
  where
    found = \case
      LocalAssum (Binder (Just v)) τ   | v == target -> Just τ
      LocalDef   v                 τ _ | v == target -> Just τ
      _ -> Nothing

boundNames :: LocalContext α ν -> [ν]
boundNames (LocalContext γ) = catMaybes $ map nameOf γ
