{-# language FlexibleContexts #-}

module Diff.GlobalDeclaration
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
import           Term.Variable
import           Typing.GlobalDeclaration

data Diff α
  = Same
  | ChangeGlobalAssum (DA.Diff Variable) (DT.Diff α)
  | ChangeGlobalDef   (DA.Diff Variable) (DT.Diff α) (DT.Diff α)
  | ChangeGlobalInd   (DI.Diff α)
  deriving (Show)

patch ::
  ( Member (Exc String) r
  ) => GlobalDeclaration α Variable -> Diff α -> Eff r (GlobalDeclaration α Variable)
patch gd δgd =
  let exc reason = throwExc $ printf "Diff.GlobalDeclaration/patch: %s" reason in
  case δgd of

    Same -> return gd

    ChangeGlobalAssum δv δτ ->
      case gd of
        GlobalAssum v τ -> GlobalAssum <$> DA.patch v δv <*> DT.patch τ δτ
        _               -> exc $ "ChangeGlobalAssum: not a GlobalAssum"

    ChangeGlobalDef δv δt δτ ->
      case gd of
        GlobalDef v t τ -> GlobalDef <$> DA.patch v δv <*> DT.patch t δt <*> DT.patch τ δτ
        _               -> exc $ "ChangeGlobalDef: not a GlobalDef"

    ChangeGlobalInd δind ->
      case gd of
        GlobalInd ind -> GlobalInd <$> DI.patch ind δind
        _               -> exc $ "ChangeGlobalInd: not a GlobalInd"

