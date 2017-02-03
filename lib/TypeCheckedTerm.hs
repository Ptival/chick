{-# LANGUAGE RankNTypes #-}
--{-# language LambdaCase #-}
{-# language TypeFamilies #-}

module TypeCheckedTerm where

import RawTerm
import Term

data TypeChecked

type TypeCheckedTerm = TermX TypeChecked
type TypeCheckedType = TypeCheckedTerm

-- it'd be nice for this to be
-- forall ξ. TypeX ξ
-- but I'm not sure how to make this happen
type TypeAnnotation = RawType

type instance X_Annot TypeChecked = TypeAnnotation
type instance X_App   TypeChecked = TypeAnnotation
type instance X_Hole  TypeChecked = TypeAnnotation
type instance X_Lam   TypeChecked = TypeAnnotation
type instance X_Let   TypeChecked = TypeAnnotation
type instance X_Pi    TypeChecked = TypeAnnotation
type instance X_Type  TypeChecked = TypeAnnotation
type instance X_Var   TypeChecked = TypeAnnotation
