{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language TypeFamilies #-}
{-# language TypeSynonymInstances #-}
{-# language UndecidableInstances #-}

module Term.TypeErrored where

--import Control.Monad.Identity

--import           DictMeta
--import           DictMetaHead
--import           DictMetaId
--import           DictMetaMap
--import           DictMetaMapId
import qualified Term.Raw            as Raw
import           Term.Term
import qualified Term.TypeChecked    as TypeChecked
import           TypeCheckingFailure

data TypeErrored ν

type Term ν = TermX (TypeErrored ν) ν
type Type ν = Term ν

type TypeError ν tChecked = Either (TypeCheckingFailure Raw.Term ν) tChecked

type instance X_Annot (TypeErrored ν) = TypeError ν (X_Annot (TypeChecked.TypeChecked ν))
type instance X_App   (TypeErrored ν) = TypeError ν (X_App   (TypeChecked.TypeChecked ν))
type instance X_Hole  (TypeErrored ν) = TypeError ν (X_Hole  (TypeChecked.TypeChecked ν))
type instance X_Lam   (TypeErrored ν) = TypeError ν (X_Lam   (TypeChecked.TypeChecked ν))
type instance X_Let   (TypeErrored ν) = TypeError ν (X_Let   (TypeChecked.TypeChecked ν))
type instance X_Pi    (TypeErrored ν) = TypeError ν (X_Pi    (TypeChecked.TypeChecked ν))
type instance X_Type  (TypeErrored ν) = TypeError ν (X_Type  (TypeChecked.TypeChecked ν))
type instance X_Var   (TypeErrored ν) = TypeError ν (X_Var   (TypeChecked.TypeChecked ν))

uncheckedDict :: DictMetaMapId ξ (TypeErrored ν)
uncheckedDict = DictMetaMap f f f f f f f f
  where
    f :: a -> Identity (TypeError ν b)
    f _ = pure (Left Unchecked)

unchecked :: TermX ξ ν -> Term ν
unchecked = metaMapId uncheckedDict

{-
fromCheckedDict :: DictMetaMapId (TypeChecked.TypeChecked ν) (TypeErrored ν)
fromCheckedDict =
  DictMetaMap f f f f f f f f
  where
    f :: Identity a -> Identity (TypeError ν a)
    f = fmap Right

fromChecked :: TypeChecked.Term ν -> Term ν
fromChecked = metaMapId fromCheckedDict

{-
`annotateError e term` marks the entire `term` unchecked, and marks its head
with the error `e`
-}
annotateError :: TypeCheckingFailure Raw.Term ν -> TermX ξ ν -> Term ν
annotateError e term =
  metaIdHead (DictMeta x x x x x x x x) $ unchecked term
  where
    x :: Identity (TypeError a)
    x = pure (Left e)

getTypeError :: Term ν -> String
getTypeError = metaHead (DictMetaHead f f f f f f f f)
  where
    f :: Show a => a -> String
    f = show
-}
