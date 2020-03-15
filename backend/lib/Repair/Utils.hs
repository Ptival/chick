module Repair.Utils (
  Repair,
  RepairTermType,
  RepairTermType'',
  lookupType,
  findDeclarationDiff,
  findGlobalDeclarationDiff,
  unpackDeclarationDiff,
  ) where

import           Control.Applicative            ( (<|>) )
import           Polysemy                       ( Member, Sem )
import           Polysemy.Error                 ( Error, catch, throw )
import           Polysemy.State                 ( State )
import           Polysemy.Trace                 ( Trace )
import           Text.Printf                    ( printf )

import qualified Diff.Atom as ΔA
import qualified Diff.Binder as ΔB
import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.LocalContext as ΔLC
import qualified Diff.LocalDeclaration as ΔLD
import qualified Diff.Term as ΔT
-- import qualified Diff.Term as ΔT
import           Language                       (Language(Chick))
import           PrettyPrinting.PrettyPrintable
import           Repair.State
import qualified Term.Raw                       as Raw
import           Term.Term
import qualified Typing.GlobalEnvironment       as GE
import qualified Typing.LocalContext            as LC

type Repair r =
  ( Member (Error String) r
  , Member Trace r
  , Member (State RepairState) r
  )

type RepairTermType r =
  Repair r =>
  Raw.Term Variable ->
  Raw.Type Variable ->
  ΔT.Diff Raw.Raw ->
  Sem r (ΔT.Diff Raw.Raw)

type RepairTermType'' r =
  Repair r =>
  Raw.Term Variable ->
  Sem r (ΔT.Diff Raw.Raw)

lookupType ::
  Member (Error String)      r =>
  Member (State RepairState) r =>
  Variable -> Sem r (Raw.Type Variable)
lookupType target = do
  let
    exc :: Member (Error String) r => String -> Sem r a
    exc reason = throw (printf "Repair.Utils/lookupType: %s" reason :: String)
  (γ, _) <- getContexts
  (e, _) <- getEnvironments
  let res = LC.lookupType target γ <|> GE.lookupRawType target e
  case res of
    Nothing -> exc $ printf "Could not find %s in either local context or global environment" (prettyStr @'Chick target)
    Just τ  -> return τ

findGlobalDeclarationDiff ::
  ( Member (Error String) r
  , Member Trace r
  , Member (State RepairState) r
  ) =>
  Variable -> Sem r (ΔGD.Diff Raw.Raw)
findGlobalDeclarationDiff v = do
  (e, δe) <- getEnvironments
  ΔGE.findGlobalDeclarationDiff v e δe

findDeclarationDiff ::
  Member (Error String)      r =>
  Member Trace               r =>
  Member (State RepairState) r =>
  Variable -> Sem r (Either (ΔLD.Diff Raw.Raw) (ΔGD.Diff Raw.Raw))
findDeclarationDiff v = do
  let
    exc :: Member (Error String) r => String -> Sem r a
    exc reason = throw (printf "Repair.Utils/findDeclarationDiff: %s" reason :: String)
  (γ, δγ) <- getContexts
  (e, δe) <- getEnvironments
  (Left <$> ΔLC.findLocalDeclarationDiff  v γ δγ)
    `catch`
    (\ (localError :: String) ->
        (Right <$> ΔGE.findGlobalDeclarationDiff v e δe)
        `catch`
        (\ (globalError :: String) -> exc $ printf
          "Could not find %s in either local context or global environment:\n  > %s\n  > %s"
          (prettyStr @'Chick v) localError globalError
        )
    )

unpackDeclarationDiff ::
  Either (ΔLD.Diff Raw.Raw) (ΔGD.Diff Raw.Raw) ->
  (ΔA.Diff Variable, ΔT.Diff Raw.Raw)
unpackDeclarationDiff = \case
  Left dld ->
    case dld of
      ΔLD.Same -> (ΔA.Same, ΔT.Same)
      ΔLD.ModifyLocalAssum δb δτv ->
        let δv = ΔB.toΔVariable δb in
        (δv, δτv)
      ΔLD.ModifyLocalDef δv δτv -> (δv, δτv)
  Right dgd ->case dgd of
    ΔGD.Same                       -> (ΔA.Same, ΔT.Same)
    ΔGD.ModifyGlobalAssum δv δτv   -> (δv, δτv)
    ΔGD.ModifyGlobalDef   δv δτv _ -> (δv, δτv)
    ΔGD.ModifyGlobalInd   _        -> error "TODO if this happens 2"
