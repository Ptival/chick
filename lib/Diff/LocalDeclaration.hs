{-# language FlexibleContexts #-}

module Diff.LocalDeclaration
  ( Diff(..)
  , patch
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception

import qualified Diff.Atom as DA
import qualified Diff.Term as DT
import           Term.Variable
--import           Typing.LocalContext
import           Typing.LocalDeclaration

data Diff α
  = Same
  | Change
    (DA.Diff Variable) -- the name
    (DT.Diff α)        -- the type
  deriving (Show)

patch ::
  ( Member (Exc String) r
  ) => LocalDeclaration α Variable -> Diff α -> Eff r (LocalDeclaration α Variable)
patch ld Same = return ld
patch ld (Change dv dτ) =
  case ld of
    LocalAssum v   τ -> LocalAssum <$> DA.patch v dv              <*> DT.patch τ dτ
    LocalDef   v t τ -> LocalDef   <$> DA.patch v dv <*> return t <*> DT.patch τ dτ
