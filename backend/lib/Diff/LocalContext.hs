module Diff.LocalContext
  ( Diff,
    findLocalDeclarationDiff,
    patch,
  )
where

import qualified Diff.List as DL
import qualified Diff.LocalDeclaration as DLD
import Language (Language (Chick))
import Polysemy (Member, Sem)
import Polysemy.Error (Error, throw)
import Polysemy.Trace (Trace, trace)
import PrettyPrinting.Chick ()
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyStr),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyStrU),
  )
import Term.Variable (Variable)
import Text.Printf (printf)
import Typing.LocalContext (LocalContext (..), lookupType)
import Typing.LocalDeclaration (LocalDeclaration, nameOf)

type Diff α = DL.Diff (LocalDeclaration α Variable) (DLD.Diff α)

patch ::
  ( Member (Error String) r,
    Member Trace r,
    Show α
  ) =>
  LocalContext α Variable ->
  Diff α ->
  Sem r (LocalContext α Variable)
patch (LocalContext γ) d = do
  γ' <- DL.patch DLD.patch γ d
  return $ LocalContext γ'

findLocalDeclarationDiff ::
  ( Member (Error String) r,
    Member Trace r
  ) =>
  Variable ->
  LocalContext α Variable ->
  Diff α ->
  Sem r (DLD.Diff α)
findLocalDeclarationDiff v γ δγ =
  trace
    ( printf
        "Diff.LocalContext/findLocalDeclarationDiff: Searching %s in %s"
        (prettyStr @ 'Chick v)
        (prettyStrU @ 'Chick γ)
    )
    >> let exc :: Member (Error String) r => String -> Sem r a
           exc (reason :: String) = throw (printf "Diff.LocalContext/findLocalDeclarationDiff: %s" reason :: String)
        in case δγ of
             DL.Same ->
               case lookupType v γ of
                 Nothing -> exc $ printf "Not found: %s" (show v)
                 Just _ -> return DLD.Same
             DL.Insert _ δ ->
               -- We're looking for the "old" v, not a new one
               findLocalDeclarationDiff v γ δ
             -- if nameOf ld == Just v
             -- then exc "TODO: this might be DLD.Change, but could be we want to skip..."
             -- else findLocalDeclarationDiff v γ δ

             DL.Modify DLD.Same δ -> findLocalDeclarationDiff v γ (DL.Keep δ)
             DL.Modify dld δ ->
               case unLocalContext γ of
                 [] -> exc "DL.Change but empty context"
                 ld : γ' ->
                   if nameOf ld == Just v
                     then return dld
                     else findLocalDeclarationDiff v (LocalContext γ') δ
             DL.Permute _ _ -> exc "TODO: Permute"
             DL.Keep δ ->
               case unLocalContext γ of
                 [] -> exc "DL.Keep but empty context"
                 h : γ' ->
                   if nameOf h == Just v
                     then return DLD.Same
                     else findLocalDeclarationDiff v (LocalContext γ') δ
             DL.Remove _ -> exc "TODO: Remove"
             DL.Replace _ -> exc "TODO: Replace"
