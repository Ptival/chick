module Goal
  ( Goal(..)
  ) where

import GHC.Generics

import Term.Term
import Typing.LocalContext

data Goal ξ ν = Goal
  { hypotheses :: LocalContext ξ ν
  , conclusion :: TermX ξ ν
  }
  deriving (Generic)

deriving instance (Eq ξ, Eq ν) => Eq (Goal ξ ν)
deriving instance (Show ξ, Show ν) => Show (Goal ξ ν)

-- instance PrettyPrintableAnnotated Goal where
--   prettyDocA (Goal hyps concl) = do
--     hypsDoc <- mapM prettyDocA hyps
--     conclDoc <- prettyDocA concl
--     return $ vcat
--       [ vcat hypsDoc
--       , text (replicate 40 '-')
--       , conclDoc
--       ]
