{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Diff.Script
  ( Diff
  , patch
  ) where

import           Polysemy        ( Member, Sem )
import           Polysemy.Error  ( Error )
import           Polysemy.Trace  ( Trace )

import qualified Diff.List       as DL
import qualified Diff.Vernacular as DV
import qualified Term.Raw        as Raw
import           Term.Variable
import           Script
import           Vernacular

type Diff α = DL.Diff (Vernacular α Variable) (DV.Diff α)

patch ::
  Member (Error String) r =>
  Member Trace          r =>
  Script Raw.Raw Variable -> Diff Raw.Raw -> Sem r (Script Raw.Raw Variable)
patch (Script s) δs = Script <$> DL.patch DV.patch s δs
