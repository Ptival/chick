{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Typing.LocalContext
  ( LocalContext (..),
    TypeCheckedLocalContext,
    addHyp,
    addLocalAssum,
    boundNames,
    lookupType,
  )
where

import Control.Monad (msum)
import Control.Monad.Except (MonadError, throwError)
import Data.Maybe (mapMaybe)
import Term.Binder (Binder (Binder))
import Term.Term (TypeX)
import Term.TypeChecked (Checked)
import Typing.LocalDeclaration
  ( LocalDeclaration (LocalAssum, LocalDef),
    nameOf,
  )

newtype LocalContext α ν = LocalContext {unLocalContext :: [LocalDeclaration α ν]}
  deriving (Eq, Monoid, Semigroup, Show)

addHyp ::
  (Eq ν, MonadError String m) =>
  LocalDeclaration α ν ->
  LocalContext α ν ->
  m (LocalContext α ν)
addHyp hyp (LocalContext hyps)
  | nameOf hyp `elem` map nameOf hyps = throwError "addHyp: name conflict"
  | otherwise = return . LocalContext $ hyp : hyps

addLocalAssum :: (Binder ν, TypeX α ν) -> LocalContext α ν -> LocalContext α ν
addLocalAssum (b, τ) (LocalContext γ) = LocalContext (LocalAssum b τ : γ)

type TypeCheckedLocalContext ν = LocalContext (Checked ν) ν

lookupType :: Eq ν => ν -> LocalContext α ν -> Maybe (TypeX α ν)
lookupType target (LocalContext γ) = msum (map found γ)
  where
    found = \case
      LocalAssum (Binder (Just v)) τ | v == target -> Just τ
      LocalDef v τ _ | v == target -> Just τ
      _ -> Nothing

boundNames :: LocalContext α ν -> [ν]
boundNames (LocalContext γ) = mapMaybe nameOf γ
