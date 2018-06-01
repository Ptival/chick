{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Diff.Script
  ( Diff
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Trace

import qualified Diff.List as DL
import qualified Diff.Vernacular as DV
import qualified Term.Raw as Raw
import           Term.Variable
import           Script
import           Vernacular

type Diff α = DL.Diff (Vernacular α Variable) (DV.Diff α)

patch ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  Script Raw.Raw Variable -> Diff Raw.Raw -> Eff r (Script Raw.Raw Variable)
patch (Script s) δs = Script <$> DL.patch DV.patch s δs
