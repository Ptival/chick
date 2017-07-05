module Diff.LocalDeclaration where

import qualified Diff.Term as DT
import           Typing.LocalContext

data Diff α
  = Same
