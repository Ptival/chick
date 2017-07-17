{-# LANGUAGE FlexibleContexts #-}

module Diff.Vernacular
  ( Diff(..)
  , patch
  ) where

import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.Inductive as DI
import qualified Diff.Term as DT
import           Diff.Utils
import qualified Term.Raw as Raw
import           Term.Variable
import           Vernacular

data Diff α
  = Same
  | ChangeDefinition (DA.Diff Variable) (DT.Diff α) (DT.Diff α)
  | ChangeInductive (DI.Diff α)

patch ::
  Member (Exc String) r =>
  Vernacular Raw.Raw Variable -> Diff Raw.Raw -> Eff r (Vernacular Raw.Raw Variable)
patch v δv =
  let exc reason = throwExc $ printf "Diff.Vernacular/patch: %s" reason in
  case δv of
    Same -> return v
    ChangeDefinition δn δτ δt ->
      case v of
        Definition n τ t -> Definition <$> DA.patch n δn <*> DT.patch τ δτ <*> DT.patch t δt
        _ -> exc "ChangeDefinition: not a Definition"
    ChangeInductive δind ->
      case v of
        Inductive ind -> Inductive <$> DI.patch ind δind
        _ -> exc "ChangeInductive: not an Inductive"
