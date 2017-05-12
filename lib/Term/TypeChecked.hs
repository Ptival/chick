{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Term.TypeChecked where

--import Data.Default
import GHC.Generics

import           Term.Term
--import qualified Term.Raw  as Raw

data Checked ν = Checked (TermX (Checked ν) ν)
  deriving (Eq, Generic, Show)

type Term ν = TermX (Checked ν) ν
type Type ν = Term ν

typeOf :: Term ν -> Maybe (Checked ν)
typeOf = annotationOf
