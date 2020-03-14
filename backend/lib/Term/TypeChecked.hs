module Term.TypeChecked where

import           GHC.Generics

import           Term.Term
import qualified Term.Universe as U

-- need data here to allow the recursion
newtype Checked ν = Checked { unChecked :: TermX (Checked ν) ν }
  deriving (Eq, Generic, Show)

type Annotation ν = Checked ν
type Term ν = TermX (Checked ν) ν
type Type ν = Term ν

typeOf :: Term ν -> Maybe (Term ν)
typeOf (Type _) = Just (Type U.Type)
typeOf t        = unChecked <$> annotationOf t
