{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Guess.Constructor
  ( guess,
    telescopeDiffToListDiff,
    telescopeDiffToListDiffVariable,
  )
where

import Control.Monad ()
import qualified Diff.Atom as ΔA
import qualified Diff.Constructor as ΔC
import qualified Diff.Guess.Atom as ΔGA
import qualified Diff.Guess.Term as ΔGT
import qualified Diff.List as ΔL
import qualified Diff.Pair as Δ2
import qualified Diff.Term as ΔT
import qualified Diff.Triple as Δ3
import Inductive.Inductive
  ( Constructor (Constructor),
    applyConstructorIndices,
    quantifyConstructorParameters,
  )
import Language (Language (Chick))
import Polysemy (Member, Sem)
import Polysemy.Trace (Trace, trace)
import PrettyPrinting.PrettyPrintable (PrettyPrintable (preview))
import PrettyPrinting.Term ()
import qualified Term.Raw as Raw
import Term.Term
  ( Binder (unBinder),
    TermX (App, Pi, Var),
    Variable,
    unscopeTerm,
  )
import Text.Printf (printf)
import Prelude hiding (product)

{- This is quite specific.  It takes a diff for a telescope, and turns it into a
list of diffs for the parts of the telescope.  It should not concern itself with
things beyond the telescope, I think.

We need the second term because our term diff does a bad job at computing binder
diffs, because it relies on a generated telescope, where binders are droppped
when they don't bind anything.  We fix it here by checking binders in CpyPi.  In
practice, we should probably move away from using the bound library due to many
such issues in different parts of the code.
-}
telescopeDiffToListDiff ::
  Show α =>
  TermX α Variable ->
  ΔT.Diff α ->
  TermX α Variable ->
  ΔL.Diff
    (α, Binder Variable, TermX α Variable)
    (Δ3.Diff (ΔA.Diff α) (ΔA.Diff (Binder Variable)) (ΔT.Diff α))
telescopeDiffToListDiff = go
  where
    this :: String = "telescopeDiffToListDiff"
    go t δt t' = case (δt, t, t') of
      (ΔT.Same, _, _) -> ΔL.Same
      (ΔT.InsPi α δ1 b δ2, _, Pi _ _ bτ2') ->
        let τ = case ΔT.patchMaybe t δ1 of
              Nothing -> error $ printf "patch failed in %s" this
              Just p -> p
         in let (_, τ2') = unscopeTerm bτ2'
             in ΔL.Insert (α, b, τ) (go t δ2 τ2')
      (ΔT.CpyPi _ _ δ2, Pi _ _ bτ2, Pi _ _ bτ2') ->
        let (b, τ2) = unscopeTerm bτ2
         in let (b', τ2') = unscopeTerm bτ2'
             in if b == b'
                  then ΔL.Keep (go τ2 δ2 τ2')
                  else ΔL.Modify (Δ3.Modify ΔA.Same (ΔA.Replace b') ΔT.Same) (go τ2 δ2 τ2')
      (ΔT.RemovePi δ2, Pi _ _ bτ2, _) ->
        let (_, τ2) = unscopeTerm bτ2
         in ΔL.Remove (go τ2 δ2 t')
      (ΔT.Replace (Var _ _), _, _) -> ΔL.Replace []
      _ -> error $ printf "TODO %s: %s" this (show δt)

telescopeDiffToListDiffVariable ::
  Show α =>
  TermX α Variable ->
  ΔT.Diff α ->
  ΔL.Diff (α, Variable, TermX α Variable) (Δ3.Diff d e f)
telescopeDiffToListDiffVariable = go
  where
    this :: String = "telescopeDiffToListDiff"
    go t δt = case (δt, t) of
      (ΔT.Same, _) -> ΔL.Same
      (ΔT.InsPi α δ1 b δ2, _) ->
        let v = case unBinder b of
              Nothing -> error "telescopeDiffToListDiffVariable: parameters can not be underscore"
              Just vv -> vv
         in let τ = case ΔT.patchMaybe t δ1 of
                  Nothing -> error $ printf "patch failed in %s" this
                  Just t' -> t'
             in ΔL.Insert (α, v, τ) (go t δ2)
      (ΔT.CpyPi _ _ δ2, Pi _ _ bτ2) ->
        let (_, τ2) = unscopeTerm bτ2
         in ΔL.Keep (go τ2 δ2)
      (ΔT.RemovePi δ2, Pi _ _ bτ2) ->
        let (_, τ2) = unscopeTerm bτ2
         in ΔL.Remove (go τ2 δ2)
      (ΔT.Replace (Var _ _), _) -> ΔL.Replace []
      _ -> error $ printf "TODO %s: %s" this (show δt)

{-
This one is also annoying.  The idea is that we receive a diff from `f a b c` to
`f c b`, and we want to turn that into a diff from `[a, b, c]` to `[c, b]`.
It's useful because it lets us compute a diff for the list of indices of an
inductive by re-using the term-diff algorithm.

But it's hard because things like `ΔT.Same` need to be translated into the
proper number of `ΔL.Keep`.  For instance:

`f a b`   --->   `f a b c`
δt = ΔT.InsApp ΔT.Same (ΔT.Replace c)
but we need:
δl = ΔL.Keep $ ΔL.Keep $ ΔL.Insert c $ ΔL.Same
     ^^^^^^^^^^^^^^^^^ there's no information about those in δt
-}
nestedApplicationsDiffToListDiff ::
  Show α =>
  TermX α Variable ->
  ΔT.Diff α ->
  ΔL.Diff (α, TermX α Variable) (Δ2.Diff d e)
nestedApplicationsDiffToListDiff = go ΔL.Same
  where
    this :: String = "nestedApplicationsDiffToListDiff"
    go δacc t δt = case (δt, t) of
      (ΔT.InsApp α δf δarg, _) ->
        let arg = case ΔT.patchMaybe t δarg of
              Nothing -> error $ printf "patch failed in %s" this
              Just t' -> t'
         in go (ΔL.Insert (α, arg) δacc) t δf
      (ΔT.RemoveApp δf, App _ f _) -> go (ΔL.Remove δacc) f δf
      -- this is just renaming the function?
      (ΔT.Replace (Var _ _), Var _ _) -> δacc
      (ΔT.Replace (Var _ _), App _ f _) -> go (ΔL.Remove δacc) f δt
      (ΔT.Same, Var _ _) -> δacc
      _ -> error $ printf "TODO %s: (%s, %s)" this (preview @ 'Chick δt) (preview @ 'Chick t)

-- | Given two versions of a constructor, construct a guess for the appropriate
-- diff.
guess ::
  Member Trace r =>
  Constructor Raw.Raw Variable ->
  Constructor Raw.Raw Variable ->
  Sem r (ΔC.Diff Raw.Raw)
guess c1@(Constructor _ n1 cps1 cis1) c2@(Constructor _ n2 cps2 cis2) =
  if c1 == c2
    then return ΔC.Same
    else do
      δn <- ΔGA.guess n1 n2

      let uniqueVar = Var Nothing "__UNIQUE__"

      let cpsTerm1 = quantifyConstructorParameters cps1 uniqueVar
      let cpsTerm2 = quantifyConstructorParameters cps2 uniqueVar
      δcpsType <- ΔGT.guess cpsTerm1 cpsTerm2
      let δcps = telescopeDiffToListDiff cpsTerm1 δcpsType cpsTerm2
      trace "Guess for δcps"
      trace $ show δcps

      -- definitely a list diff-er for those
      let cisTerm1 = applyConstructorIndices cis1 uniqueVar
      let cisTerm2 = applyConstructorIndices cis2 uniqueVar
      δcisType <- ΔGT.guess cisTerm1 cisTerm2
      trace $ "CHECK THIS OUT"
      trace $ show δcisType
      let δcis = nestedApplicationsDiffToListDiff cisTerm1 δcisType
      trace "Guess for δcis"
      trace $ show δcis

      return $ ΔC.Modify δn δcps δcis
