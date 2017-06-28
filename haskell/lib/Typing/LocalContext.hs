{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
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

addHyp ::
  (Eq ν, MonadError String m) =>
  LocalDeclaration α ν -> LocalContext α ν -> m (LocalContext α ν)
addHyp hyp (LocalContext hyps)
  | nameOf hyp `elem` map nameOf hyps = throwError "addHyp: name conflict"
  | otherwise = return . LocalContext $ hyp:hyps

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

boundNames :: LocalContext α ν -> [ν]
boundNames (LocalContext γ) = map nameOf γ
