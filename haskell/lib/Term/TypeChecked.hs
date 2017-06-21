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

-- need data here to allow the recursion
data Checked ν = Checked { unChecked :: (TermX (Checked ν) ν) }
  deriving (Eq, Generic, Show)

type Annotation ν = Checked ν
type Term ν = TermX (Checked ν) ν
type Type ν = Term ν

typeOf :: Term ν -> Maybe (Term ν)
typeOf Type = Just $ Type
typeOf t    = unChecked <$> annotationOf t
