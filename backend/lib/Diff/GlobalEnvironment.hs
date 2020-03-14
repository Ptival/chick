module Diff.GlobalEnvironment
  ( Diff
  , findGlobalDeclarationDiff
  , findGlobalIndDiff
  , patch
  ) where

import           Polysemy                       ( Member, Sem )
import           Polysemy.Error                 ( Error, throw )
import           Polysemy.Trace                 ( Trace )
import           Text.Printf                    ( printf )

import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.Inductive as ΔI
import qualified Diff.List as ΔL
import           Inductive.Inductive
import           Language                       ( Language(Chick) )
import           PrettyPrinting.PrettyPrintable
import qualified Term.Raw                       as Raw
import           Term.Variable
import           Typing.GlobalDeclaration
import           Typing.GlobalEnvironment

type Diff α = ΔL.Diff (GlobalDeclaration α Variable) (ΔGD.Diff α)

patch ::
  ( Member (Error String) r
  , Member Trace r
  -- , PrettyPrintable α
  , Show α
  ) =>
  GlobalEnvironment α Variable -> Diff α -> Sem r (GlobalEnvironment α Variable)
patch (GlobalEnvironment e) δe = do
  e' <- ΔL.patch ΔGD.patch e δe
  return $ GlobalEnvironment e'

-- annotation has to be Raw because of inductives
-- TODO: need to decide whether this find inductives or not, so far no?
findGlobalDeclarationDiff ::
  Member (Error String) r =>
  Member Trace          r =>
  Variable -> GlobalEnvironment Raw.Raw Variable -> Diff Raw.Raw -> Sem r (ΔGD.Diff Raw.Raw)
findGlobalDeclarationDiff v e δe = do
  -- trace (printf "Diff.GlobalEnvironment/findGlobalDeclarationDiff:\nSearching %s in %s" (prettyStr v) (prettyStrU e))
  -- trace (printf "δe: %s" (prettyStr δe))
  let
    exc :: Member (Error String) r => String -> Sem r a
    exc (reason :: String) =
        throw (printf "Diff.GlobalEnvironment/findGlobalDeclarationDiff: %s" reason :: String)

  case δe of

    ΔL.Same ->
      case lookupRawType v e of
        Nothing -> exc $ printf "Not found: %s" (show v)
        Just _  -> return ΔGD.Same

    ΔL.Insert gd δ ->
      if nameOf gd == v
      then exc "TODO: this might be DGE.Modify, but could be we want to skip..."
      else findGlobalDeclarationDiff v e δ

    ΔL.Modify ΔGD.Same δ -> findGlobalDeclarationDiff v e (ΔL.Keep δ)

    ΔL.Modify dgd δ ->
      case unGlobalEnvironment e of
        []    -> exc "ΔL.Modify but empty environment"
        h : e' ->
          case dgd of

            ΔGD.Same -> error "This should already be matched ealier..."

            ΔGD.ModifyGlobalAssum _δv _δτ ->
              if nameOf h == v --v' == v
              then return dgd
              else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

            ΔGD.ModifyGlobalDef _δv _δτ _ ->
              if nameOf h == v --v' == v
              then return dgd
              else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

            ΔGD.ModifyGlobalInd _δind ->
              findGlobalDeclarationDiff v (GlobalEnvironment e') δ

    ΔL.Permute _ _ -> exc "TODO: Permute"

    ΔL.Keep δ ->
      case unGlobalEnvironment e of
        []    -> exc "ΔL.Keep but empty context"
        h : e' ->
          if nameOf h == v
          then return ΔGD.Same
          else findGlobalDeclarationDiff v (GlobalEnvironment e') δ

    ΔL.Remove _ -> exc "TODO: Remove"

    ΔL.Replace _ -> exc "TODO: Replace"

-- Assumption: the ind existed before, and we're just trying to find how it was
-- modified
findGlobalIndDiff ::
  ( Eq ν
  , Member (Error String) r
  , PrettyPrintable 'Chick α
  ) =>
  Inductive α ν -> GlobalEnvironment ξ ν -> Diff α ->
  Sem r (ΔI.Diff α)
findGlobalIndDiff ind e δe = do
  let
    exc :: Member (Error String) r => String -> Sem r a
    exc (reason :: String) =
        throw (printf "Diff.GlobalEnvironment/findGlobalIndDiff: %s" reason :: String)
  case (δe, unGlobalEnvironment e) of
    (ΔL.Insert _ δ, _) -> findGlobalIndDiff ind e δ
    (ΔL.Keep δ, GlobalInd ind' : e') ->
      if inductiveName ind' == inductiveName ind
      then return ΔI.Same
      else findGlobalIndDiff ind (GlobalEnvironment e') δ
    (ΔL.Keep δ, _ : e') ->
      findGlobalIndDiff ind (GlobalEnvironment e') δ
    (ΔL.Modify (ΔGD.ModifyGlobalInd δind) δ, GlobalInd ind' : e') ->
      if inductiveName ind' == inductiveName ind
      then return δind
      else findGlobalIndDiff ind (GlobalEnvironment e') δ
    (ΔL.Modify _ δ, _ : e') ->
      findGlobalIndDiff ind (GlobalEnvironment e') δ
    _ -> exc $ prettyStr @'Chick δe
