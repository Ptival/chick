{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Repair.Term.Branch (
  repairBranches,
  ) where

import           Control.Arrow                  ( (>>>) )
import           Control.Lens                   hiding ( preview )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )
import           Polysemy                       ( Member, Sem )
import           Polysemy.Error                 ( Error )
import           Polysemy.State                 ( State )
import           Polysemy.Trace                 ( Trace, trace )
import           Text.Printf                    ( printf )

import qualified Diff.Atom as ΔA
import qualified Diff.Constructor as ΔC
--import qualified Diff.GlobalDeclaration as ΔGD
--import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.Inductive as ΔI
import qualified Diff.LocalContext as ΔLC
import qualified Diff.LocalDeclaration as ΔLD
import qualified Diff.List as ΔL
import qualified Diff.Maybe as ΔM
import qualified Diff.Term as ΔT
import qualified Diff.Pair as Δ2
import qualified Diff.Triple as Δ3
import           Diff.Utils
import           Inductive.Inductive
import           Language                       (Language(Chick))
import           PrettyPrinting.PrettyPrintable
--import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Repair.State                   as RS
import           Repair.Utils
import           Term.Binder
import           Term.Term
-- import qualified Term.TypeChecked as C
import qualified Term.Raw                       as Raw
--import qualified Term.Universe as U
--import qualified Typing.GlobalEnvironment as GE

repairBranch ::
  Repair r =>
  RepairTermType'' r ->
  Branch Raw.Raw Variable -> Constructor Raw.Raw Variable ->
  ΔI.Diff Raw.Raw -> ΔC.Diff Raw.Raw ->
  Sem r (ΔT.BranchDiff Raw.Raw)
repairBranch repairTerm b c@(Constructor _ _ cps _) δi δc = do

  trace ">>> repairBranch"
  trace $ printf "> %s" (preview @'Chick b)
  trace $ printf "> %s" (preview @'Chick c)

  let (_, args, guardbody) = unpackBranch b
  let nbIps = length . inductiveParameters . constructorInductive $ c

  let (δctor, δcps) =
        case δc of
        ΔC.Same             -> (ΔA.Same, ΔL.Same)
        ΔC.Modify δn δcps _ -> (δn, δcps)

  -- for `withState`, we only care about parameters to the constructors, as they are
  -- the only ones that may bind, so we drop the parameters to the inductive type
  let cpArgs = drop nbIps args
  let δcpArgs = bimap extractCpL extractCpR δcps

  -- δargs is composed of two parts, reflecting:
  -- - the changes made to the inductive type parameters
  -- - the changes made to the constructor parameters
  let δips =
        case δi of
        ΔI.Same                 -> ΔL.Same
        ΔI.Modify  _ δips _ _ _ -> δips
  let δipArgs = bimap extractIpL extractIpR δips
  let δargs = merge nbIps δipArgs δcpArgs

  δbody <- withStateFromConstructorArgs cpArgs δcpArgs cps δcps
           $ repairTerm (branchBody guardbody)
  return $ Δ3.Modify δctor δargs (Δ2.Modify ΔM.Same δbody)

  where

    extractCpL :: Φcp Raw.Raw Variable -> Binder Variable
    extractCpL = view cpBinder

    extractCpR :: ΔC.Δcp Raw.Raw -> ΔA.Diff (Binder Variable)
    extractCpR Δ3.Same            = ΔA.Same
    extractCpR (Δ3.Modify _ δv _) = δv

    extractIpL :: Φip Raw.Raw Variable -> Binder Variable
    extractIpL = Binder . Just <$> view ipBinder

    extractIpR :: ΔI.Δip Raw.Raw -> ΔA.Diff (Binder Variable)
    extractIpR Δ3.Same            = ΔA.Same
    extractIpR (Δ3.Modify _ δv _) = Binder . Just <$> δv

    merge ::
      Int ->
      ΔL.Diff (Binder Variable) (ΔA.Diff (Binder Variable)) ->
      ΔL.Diff (Binder Variable) (ΔA.Diff (Binder Variable)) ->
      ΔL.Diff (Binder Variable) (ΔA.Diff (Binder Variable))
    merge 0     ΔL.Same         δcpsAs = δcpsAs
    merge 0     δipsAs          _      = error $ printf "TODO %s" (preview @'Chick δipsAs)
    merge nbIps (ΔL.Keep δipAs) δcpAs = ΔL.Keep $ merge (nbIps - 1) δipAs   δcpAs
    merge nbIps ΔL.Same         δcpAs = ΔL.Keep $ merge (nbIps - 1) ΔL.Same δcpAs
    merge as δipAs δcpAs = error $ printf "merge %s %s %s" (show as) (preview @'Chick δipAs) (preview @'Chick δcpAs)

{- This function adds the state we would expect from entering a branch in a
match construct.  cpArgs is the list of binders passed to the branch, i.e.:

  | Cons _ a _ b c => (* _, a, _, b, c *)

For each of these, we want to bring in scope the variable, and its type.
-}
withStateFromConstructorArgs :: ∀ r a.
  Repair r =>
  [Binder Variable] ->
  ΔL.Diff (Binder Variable) (ΔA.Diff (Binder Variable)) ->
  Φcps Raw.Raw Variable ->
  ΔC.Δcps Raw.Raw -> Sem r a -> Sem r a
withStateFromConstructorArgs cpArgs δcpArgs cps δcps e =
  go cpArgs (map (view cpType) cps) δcpArgs δcps
  where

    go ::
      [Binder Variable] -> [Raw.Term Variable] ->
      ΔL.Diff (Binder Variable) (ΔA.Diff (Binder Variable)) ->
      ΔC.Δcps Raw.Raw -> Sem r a

    go [] [] _ _ = e

    go (a : as) (cp : cps) δargs δcps = do

      let (δassum, δargs', δcps') = nextδassum (δargs, δcps)

      trace "*** Adding local assumption:"
      trace $ printf "(%s, %s)" (preview @'Chick a) (preview @'Chick cp)
      trace "*** Adding delta local assumption:"
      trace $ printf "%s" (prettyStr @'Chick $ δassum ΔL.Same)

      RS.withLocalAssum (a, cp) >>> RS.withδLocalContext δassum $ do
        RS.sanityCheck
        go as cps δargs' δcps'

    go _ _ _ _ = error "different lengths in withStateFromConstructors"

    {- nextδassum finds the next important assumption modifier we want to add to
    the global environment.  -}

    nextδassum ::
      ( ΔL.Diff (Binder Variable) (ΔA.Diff (Binder Variable))
      , ΔL.Diff (Φcp Raw.Raw Variable) (ΔC.Δcp Raw.Raw)
      ) ->
      ( ΔLC.Diff Raw.Raw -> ΔLC.Diff Raw.Raw
      , ΔL.Diff (Binder Variable) (ΔA.Diff (Binder Variable))
      , ΔL.Diff (Φcp Raw.Raw Variable) (ΔC.Δcp Raw.Raw)
      )
    nextδassum (δargs, δcps) = case (δargs, δcps) of

      (ΔL.Insert _ δargs', ΔL.Insert _ δcps') ->
        -- we skip the inserted ones, because they don't bind
        nextδassum (δargs', δcps')

      (ΔL.Keep δargs', ΔL.Keep δps') -> (ΔL.Keep, δargs', δps')

      (ΔL.Modify δa δargs', ΔL.Modify δp δps') ->
        ( ΔL.Modify (ΔLD.ModifyLocalAssum δa (Δ3.extract3 ΔT.Same δp))
        , δargs', δps')

      (ΔL.Remove δargs', ΔL.Remove δps') -> (ΔL.Remove, δargs', δps')

      (ΔL.Same, ΔL.Same) -> (ΔL.Keep, ΔL.Same, ΔL.Same)

      _ -> error $ printf "TODO: %s, %s" (preview @'Chick δargs) (preview @'Chick δcps)

repairBranches :: ∀ r.
  Repair r =>
  RepairTermType'' r ->
  [Branch Raw.Raw Variable] ->
  Inductive Raw.Raw Variable ->
  ΔI.Diff Raw.Raw ->
  Sem r (ΔT.BranchesDiff Raw.Raw)
repairBranches _repairTerm _bs _ind ΔI.Same =
  -- repairBranches repairTerm bs ind (ΔI.Modify _ _ _ _ δcs)
  -- FIXME: fix body of branches
  return ΔL.Same
repairBranches repairTerm bs ind δi@(ΔI.Modify _ _ _ _ δcs) = do
  let cs = inductiveConstructors ind
  -- the original branches are in some permutation of the original list of constructors,
  -- let's build a permutation from and back to that order
  let (pMatchToInd, pIndToMatch) =
        computePermutations
        (map branchConstructor bs)
        (map constructorName   cs)
  let sortedBranches = permute pMatchToInd bs
  (sortedDeltas, inserts) <- go ([], []) (sortedBranches, cs, δcs)
  trace $ printf "sorted deltas: %s" (show $ map ($ ΔL.Same) sortedDeltas)
  trace $ printf "inserts:       %s" (show $ map ($ ΔL.Same) inserts)
  return $ foldr (.) id (permute pIndToMatch sortedDeltas ++ inserts) ΔL.Same

  where

    go ::
      Member (Error String)         r =>
      Member Trace                  r =>
      Member (State RS.RepairState) r =>
      ( [ ΔT.BranchesDiff Raw.Raw -> ΔT.BranchesDiff Raw.Raw ]
      , [ ΔT.BranchesDiff Raw.Raw -> ΔT.BranchesDiff Raw.Raw ]
      ) ->
      ( [Branch Raw.Raw Variable]
      , [Constructor Raw.Raw Variable]
      , ΔL.Diff (Constructor Raw.Raw Variable) (ΔC.Diff Raw.Raw)
      ) ->
      Sem r
      ( [ ΔT.BranchesDiff Raw.Raw -> ΔT.BranchesDiff Raw.Raw ]
      , [ ΔT.BranchesDiff Raw.Raw -> ΔT.BranchesDiff Raw.Raw ]
      )
    go (δbs, δins) = \case
      (bs, cs, ΔL.Insert c δcs) -> do
        let b = packBranch
                ( constructorName c
                , replicate (length (constructorParameters c)) (Binder Nothing)
                , GuardAndBody Nothing (Hole ())
                )
        trace "Inserting branch:"
        trace $ preview @'Chick b
        go (δbs, δins ++ [ΔL.Insert b]) (bs, cs, δcs)
      (b : bs, c : cs, ΔL.Modify δc δcs) -> do
        δb <- repairBranch repairTerm b c δi δc
        go (δbs ++ [ΔL.Modify δb], δins) (bs, cs, δcs)
      (     _,     cs, ΔL.Same) ->
        return (δbs ++ replicate (length cs) ΔL.Keep, δins)
      (bs, cs, δcs) ->
        error $ printf "FIXME NOW: (%s, %s, %s)" (preview @'Chick bs) (preview @'Chick cs) (preview @'Chick δcs)

-- permutations l r returns a pair (plr, prl) s.t.
-- permute plr l = r
-- permute prl r = l
-- Input:    l = [0, 1, 2, 3, 4]   r = [4, 2, 3, 0, 1]
-- Output: plr = [4, 2, 3, 0, 1] prl = [3, 4, 1, 2, 0]
computePermutations :: Eq a => [a] -> [a] -> ([Int], [Int])
computePermutations l r = go l r
  where
    go []        []        = ([], [])
    go (hl : tl) (hr : tr) = (lindex hr : tl', rindex hl : tr')
      where (tl', tr') = go tl tr
    go _ _ = error "computePermutations expect two lists of same length"
    lindex i = fromJust $ elemIndex i l
    rindex i = fromJust $ elemIndex i r
