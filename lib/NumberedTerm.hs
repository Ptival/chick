{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies #-}

module NumberedTerm where

--import Control.Lens

import RawTerm
import Term

data Numbered

type NumberedTerm = TermX Numbered
type NumberedType = NumberedTerm

type Numbering = Int

type instance X_Annot Numbered = Numbering
type instance X_App   Numbered = Numbering
type instance X_Hole  Numbered = Numbering
type instance X_Lam   Numbered = Numbering
type instance X_Let   Numbered = Numbering
type instance X_Pi    Numbered = Numbering
type instance X_Type  Numbered = Numbering
type instance X_Var   Numbered = Numbering

numberOf :: NumberedTerm -> Int
numberOf = annotationOf

{-
metaFold ::
forall ξ ψ a. DictMetaFold ξ ((,) a) ψ ((,) a) -> a -> TermX ξ -> TermX ψ
-}

numberize :: RawTerm -> NumberedTerm
numberize = metaFold (dictMetaFold' (\ (acc, ()) -> (acc + 1, acc))) 0
