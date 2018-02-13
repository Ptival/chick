{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module Repair.Term where

import           Control.Arrow
import           Control.Lens hiding (preview)
--import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Data.Maybe
import           Data.List
import           Text.Printf

import qualified Diff.Atom as ΔA
import qualified Diff.Constructor as ΔC
--import qualified Diff.GlobalDeclaration as ΔGD
import qualified Diff.GlobalEnvironment as ΔGE
import qualified Diff.Inductive as ΔI
import qualified Diff.LocalContext as ΔLC
import qualified Diff.LocalDeclaration as ΔLD
import qualified Diff.List as ΔL
import qualified Diff.Term as ΔT
import qualified Diff.Triple as Δ3
import           Diff.Utils
import           Inductive.Inductive
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Repair.State as RS
import           Repair.Utils
import           Term.Binder
import           Term.Term
-- import qualified Term.TypeChecked as C
import qualified Term.Raw as Raw
import qualified Term.Universe as U
import qualified Typing.GlobalEnvironment as GE

-- | For example:
-- | τ  = A → C → D                             t  = (f a) c
-- | τ' = A → (B → (C -> D))                    t' = ((f a) _) c
-- | Δ  = CpyP Same (InsP B (CpyP Same Same))   δ  = CpyA (InsA (CpyA Same Same) InsH) Same
-- |      ^^^^                                                   ^^^^
-- |                 ^^^^                                  ^^^^
-- |                         ^^^^                    ^^^^

-- | `repairArgs args τ δτ δfun`
-- | `args` is the list of arguments prior to changes
-- | `τ`    is the type of the remaining Pi-telescope
-- | `δτ`   is the diff for `τ` to become the new telescope
-- | `δfun` is the diff to apply to the actual function, which is the base of this "fold"
repairArgs ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  [Raw.Term Variable] ->
  Raw.Type Variable ->
  ΔT.Diff Raw.Raw ->
  ΔT.Diff Raw.Raw ->
  Eff r (ΔT.Diff Raw.Raw)
repairArgs args τ0 δτ0 δfun =
  trace "Repair.Term/repairArgs with:" >>
  trace (printf "> args: %s" (prettyStr args)) >>
  trace (printf "> τ0:   %s" (prettyStr   τ0)) >>
  trace (printf "> δτ0:  %s" (prettyStr  δτ0)) >>
  trace (printf "> δfun: %s" (prettyStr δfun)) >>
  go δfun args τ0 δτ0
  where

    exc (reason :: String) = throwExc $ printf "Repair.Term/repairArgs: %s" reason

    -- go accumulates the resulting diff in an unintuitive way
    go acc args τ δτ =

      (do
        trace $ "Repair.Term/repairArgs/go:"
        trace $ printf "> args: %s" (prettyStr args)
        trace $ printf "> τ:    %s" (prettyStr τ)
        trace $ printf "> δτ:   %s" (prettyStr δτ)
        τ' <- ΔT.patch τ δτ
        trace $ printf "> τ':   %s" (prettyStr τ')
      )
      >>

      case (args, δτ) of

        ([], ΔT.CpyApp _ _) -> return acc

        --        | TYPE             | TERM
        -- BEFORE | (x : X) → Ys → R | acc x ys
        -- AFTER  | (x : X) → Zs → R | acc x zs
        (arg : args, ΔT.CpyPi δ1 _ δ2) -> do
          (_, τ1, _, τ2) <- ΔT.extractPi τ
          δarg <- repair arg τ1 δ1
          go (ΔT.CpyApp acc δarg) args τ2 δ2

        ([], ΔT.CpyPi _ _ _) ->
          -- this happens when the function was partially applied
          return acc

        ([], ΔT.CpyVar _) -> return acc

        ([], ΔT.InsApp _ _ _) -> return acc -- TODO: can we do better here?

        --        | TYPE                  | TERM
        -- BEFORE | Xs →         → Zs → R | f xs zs
        -- AFTER  | Xs → (y : Y) → Zs → R | f xs y zs
        (_, ΔT.InsPi _ δ1 _ δ2) ->
          go (ΔT.InsApp () acc (hole δ1)) args τ δ2

        --        | TYPE             | TERM
        -- BEFORE | A1 → A2 → Bs → R | acc a1 a2 bs
        -- AFTER  | A2 → A1 → Bs → R | acc a2 a1 bs
        (_, ΔT.PermutPis p δτ') -> do
          let args' = permute p args
          ΔT.PermutApps p <$> go acc args' τ δτ'
        -- TODO: this is wrong because we need the permutations to happen from within the
        -- innermost App rather than the outermost ones

        (_, ΔT.Same) -> do
          -- even though it's Same, we still need to peel off ∀s from τ
          -- before returning acc
          trace $ printf "τ: %s" (prettyStr τ)
          case (args, τ) of
            (_ : args, Pi _ _ bτ') ->
              let (_, τ') = unscopeTerm bτ' in
              go (ΔT.CpyApp acc ΔT.Same) args τ' ΔT.Same
            _         -> return acc

        (_, ΔT.Replace τ') -> do
          -- if this happens, we have no reason to believe that the remaining arguments
          -- are relevant to the replacement type: we need to drop them all, and then
          -- insert as many arguments as the replacement type calls for
          pis <- ΔT.extractPis τ'
          return
            . ΔT.nInsertApps (length pis) ((), Hole ())
            . ΔT.nRemoveApps (length args)
            $ acc

        _ -> exc $ printf "repairArgs, TODO:\nargs: %s\nτ: %s\nδτ: %s"
             (show args) (show τ) (show δτ)

    hole = \case
      ΔT.Replace τ' -> ΔT.Replace (Annot () (Hole ()) τ')
      _             -> ΔT.Replace (Hole ())

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

{- This function adds the state we would expect from entering a branch in a
match construct.  cpArgs is the list of binders passed to the branch, i.e.:

  | Cons _ a _ b c => (* _, a, _, b, c *)

For each of these, we want to bring in scope the variable, and its type.
-}
withStateFromConstructorArgs :: ∀ r a.
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  [Binder Variable] ->
  ΔL.Diff (Binder Variable) (ΔA.Diff (Binder Variable)) ->
  Φcps Raw.Raw Variable ->
  ΔC.Δcps Raw.Raw -> Eff r a -> Eff r a
withStateFromConstructorArgs cpArgs δcpArgs cps δcps e =
  go cpArgs (map (view cpType) cps) δcpArgs δcps
  where

    go ::
      [Binder Variable] -> [Raw.Term Variable] ->
      ΔL.Diff (Binder Variable) (ΔA.Diff (Binder Variable)) ->
      ΔC.Δcps Raw.Raw -> Eff r a

    go [] [] _ _ = e

    go (a : as) (cp : cps) δargs δcps = do

      let (δassum, δargs', δcps') = nextδassum (δargs, δcps)

      trace "*** Adding local assumption:"
      trace $ printf "(%s, %s)" (preview a) (preview cp)
      trace "*** Adding delta local assumption:"
      trace $ printf "%s" (prettyStr $ δassum ΔL.Same)

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

      (ΔL.Same, ΔL.Same) ->
        -- TODO: double-check this
        (ΔL.Keep, ΔL.Same, ΔL.Same)

      _ -> error $ printf "TODO: %s, %s" (preview δargs) (preview δcps)

repairBranch ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  Branch Raw.Raw Variable -> Constructor Raw.Raw Variable ->
  ΔI.Diff Raw.Raw -> ΔC.Diff Raw.Raw ->
  Eff r (ΔT.BranchDiff Raw.Raw)
repairBranch b c@(Constructor _ _ cps _) δi δc = do

  trace ">>> repairBranch"
  trace $ printf "> %s" (preview b)
  trace $ printf "> %s" (preview c)

  let (_, args, body) = unpackBranch b
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
           $ unknownTypeRepair body
  return $ Δ3.Modify δctor δargs δbody

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
    merge 0     δipsAs          _      = error $ printf "TODO %s" (preview δipsAs)
    merge nbIps (ΔL.Keep δipAs) δcpAs = ΔL.Keep $ merge (nbIps - 1) δipAs   δcpAs
    merge nbIps ΔL.Same         δcpAs = ΔL.Keep $ merge (nbIps - 1) ΔL.Same δcpAs
    merge as δipAs δcpAs = error $ printf "merge %s %s %s" (preview as) (preview δipAs) (preview δcpAs)

repairBranches ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  [Branch Raw.Raw Variable] ->
  Inductive Raw.Raw Variable ->
  ΔI.Diff Raw.Raw ->
  Eff r (ΔL.Diff t (ΔT.BranchDiff Raw.Raw))
repairBranches _ _ ΔI.Same =
  -- FIXME: fix body of branches
  return $ ΔL.Same
repairBranches bs (Inductive _ _ _ _ cs) δi@(ΔI.Modify _ _ _ _ δcs) = do
  -- the original branches are in some permutation of the original list of constructors,
  -- let's build a permutation from and back to that order
  let (pMatchToInd, pIndToMatch) =
        computePermutations
        (map branchConstructor bs)
        (map constructorName   cs)
  let sortedBranches = permute pMatchToInd bs
  (sortedDeltas, inserts) <- go ([], []) (sortedBranches, cs, δcs)
  return $ foldr (.) id (permute pIndToMatch sortedDeltas ++ inserts) ΔL.Same
  where
    go (δbs, δins) = \case
      (     _,     cs, ΔL.Same) ->
        return (δbs ++ replicate (length cs) ΔL.Keep, δins)
      (b : bs, c : cs, ΔL.Modify δc δcs) -> do
        δb <- repairBranch b c δi δc
        go (δbs ++ [ΔL.Modify δb], δins) (bs, cs, δcs)
      _ -> error $ printf "FIXME NOW: %s" (prettyStr δcs)

guessIndMatched ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  Raw.Term Variable -> [Branch Raw.Raw Variable] -> Eff r (Inductive Raw.Raw Variable)
guessIndMatched discriminee _branches = do
  let exc (s :: String) = throwExc $ printf "Repair.Term/guessIndMatched: %s" s
  trace "*** Trying to guess matched type using discriminee"
  case discriminee of
    Var _ v -> do
      τv <- lookupType v
      (fun, _) <- ΔT.extractApps τv
      case fun of
        Var _ indName -> do
          RS.RepairState _ _ e _ <- get
          case GE.lookupInductiveByName indName e of
            Just ind -> return ind
            Nothing -> exc $ printf "Could not find %s in global environment" (prettyStr indName)
        _ -> exc "Could not guess the inductive being matched because the type of the discriminee did not look like a variable applied"
    _ -> exc "Could not guess the inductive being matched because the term being matched is not a variable (TODO)"

-- | `unknownTypeRepair t` attempts to repair `t` without any information
unknownTypeRepair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  Raw.Term Variable -> Eff r (ΔT.Diff Raw.Raw)
unknownTypeRepair t = do

  let exc (reason :: String) =
        throwExc $ printf "Repair.Term/unknownTypeRepair: %s" reason

  trace $ printf "Repair.Term/unknownTypeRepair(t: %s)" (prettyStrU t)

  case t of

    Type _ -> return ΔT.Same

    Var _ v -> do
      τv <- lookupType v
      (δv, δτv) <- unpackDeclarationDiff <$> findDeclarationDiff v
      repairArgs [] τv δτv (ΔT.CpyVar δv)

    App _ _ _ -> do
      -- (f, [x, y, z])
      (fun, args)   <- ΔT.extractApps t
      case fun of

        Var _ v -> do
          -- s <- get
          -- let γ  = view RS.context s
          -- let δγ = view RS.δcontext s
          -- γ' <- ΔLC.patch γ δγ
          τv <- lookupType v
          trace $ printf "Trying to repair an application of %s" (prettyStr v)
          trace $ printf "Looked up type: %s" (prettyStr τv)
          (δv, δτv) <- unpackDeclarationDiff <$> findDeclarationDiff v
          trace $ printf "τv: %s" (prettyStr τv)
          trace $ printf "δτv: %s" (prettyStr δτv)
          repairArgs (map snd args) τv δτv (ΔT.CpyVar δv)

        _ -> exc "repair, Same, App, Not Var"

    Pi _ τ1 bτ2 -> do
      let (b, τ2) = unscopeTerm bτ2
      RS.withLocalAssum (b, τ1) >>> RS.withδLocalContext ΔL.Keep $ do
        ΔT.CpyPi
          <$> repair τ1 (Type U.Type) ΔT.Same
          <*> pure ΔA.Same
          <*> repair τ2 (Type U.Type) ΔT.Same

    Match _ d bs -> do
      δd <- unknownTypeRepair d
      -- eventually, it'd be nice to have the type at which the match is done already
      -- figured out
      ind  <- guessIndMatched d bs
      trace $ printf "*** INDUCTIVE MATCHED: %s" (prettyStr ind)
      RS.RepairState _ _ e δe <- get
      δind <- ΔGE.findGlobalIndDiff ind e δe
      trace $ printf "*** δINDUCTIVE MATCHED: %s" (prettyStr δind)
      δbs <- repairBranches bs ind δind
      trace $ printf "*** PROPOSED: %s" (prettyStr δbs)
      return $ ΔT.CpyMatch δd δbs

    _ -> exc $ printf "YO THIS HAPPENS: %s" (prettyStr t)

-- | `genericRepair t τ` attempts to repair `t` without more information about
-- | how its type `τ` changed
genericRepair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  Raw.Term Variable -> Raw.Type Variable -> Eff r (ΔT.Diff Raw.Raw)
genericRepair t τ = do

  -- let exc (reason :: String) =
  --       throwExc $ printf "Repair.Term/genericRepair: %s" reason

  trace $ printf "Repair.Term/genericRepair(t: %s, τ: %s)"
    (prettyStrU t) (prettyStrU τ)

  case t of

    -- even though the diff is same, something inside might need updating
    Lam _ bt -> do
      let (b, tlam) = unscopeTerm bt
      (_, τ1, _, τ2) <- ΔT.extractPi τ
      -- FIXME: should we not try to repair τ1 and τ2 here?
      RS.withLocalAssum (b, τ1) >>> RS.withδLocalContext ΔL.Keep $ do
        ΔT.CpyLam ΔA.Same <$> repair tlam τ2 ΔT.Same

    _ -> unknownTypeRepair t

-- | `repair t τ δτ` assumes `t` is a term whose type is `τ` and `δτ` is a diff describing
-- | how `τ` changed.  It attempts to build a patch `δt` s.t. `patch t δt` has type `patch τ δτ`.
repair ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State RS.RepairState) r
  ) =>
  Raw.Term Variable -> Raw.Type Variable -> ΔT.Diff Raw.Raw -> Eff r (ΔT.Diff Raw.Raw)
repair t τ δτ =
  trace (printf "Repair.Term/repair(t: %s, τ: %s, δτ: %s)" (prettyStrU t) (prettyStrU τ) (show δτ)) >>
  RS.traceState >>
  let exc (reason :: String) = throwExc $ printf "Repair.Term/repair: %s" reason in

  -- (do
  --     s <- get
  --     let γ = view RS.context s
  --     trace $ printf "CONTEXT BEF:\n%s" (prettyStrU γ)
  --     γ' <- ΔLC.patch γ (view RS.contextDiff s)
  --     trace $ printf "CONTEXT AFT:\n%s" (prettyStrU γ')
  -- ) >>

  case δτ of

  -- even though the type has not changed, the term might still need updating to
  -- deal with the changes in the context
  ΔT.Same -> genericRepair t τ -- OK

  ΔT.Replace τ' -> return $ ΔT.Replace $ Annot () (Hole ()) τ'

  ΔT.CpyApp _ _ -> genericRepair t τ -- FIXME improve this?

  ΔT.CpyLam _ _ -> genericRepair t τ -- FIXME: improve this

  ΔT.CpyMatch _ _ -> genericRepair t τ -- FIXME: improve this

  ΔT.CpyVar ΔA.Same -> genericRepair t τ -- OK

  ΔT.CpyVar (ΔA.Replace δv) -> do
    trace $ printf "AT THIS POINT δv IS: %s, t IS: %s" (preview δv) (preview t)
    _ <- exc "YO FIXME"
    return $ ΔT.Replace "TODO" -- TODO: confirm this is always good

  ΔT.InsApp _ _ _ -> genericRepair t τ -- not sure what to do here

  ΔT.InsLam _ _ _   -> genericRepair t τ -- FIXME: improve this
  ΔT.PermutLams _ _ -> genericRepair t τ -- FIXME: improve this
  ΔT.PermutApps _ _ -> genericRepair t τ -- FIXME: improve this

  ΔT.CpyPi δ1 δb δ2 ->
    case (t, τ) of
      (Lam _ bt, Pi _ τ1 bτ2) -> do
        let (b, _) = unscopeTerm bt
        RS.withLocalAssumAndδ (b, τ1) (δb, δ1) $ do
          ΔT.CpyLam ΔA.Same
            <$> repair (snd $ unscopeTerm bt) (snd $ unscopeTerm bτ2) δ2
      _ -> genericRepair t τ

  ΔT.InsPi _ d1 _b d2      -> do
    -- I think what we want here is:
    -- - find a b' binder like b that is free in t
    -- - InsLam b'
    -- - recursively diff by substituting b' for b
    let varsFreeInTerm = foldr (\ v -> (v :)) [] t
    boundVarsInContext <- RS.boundVarsInContext
    trace $ printf "Variables free  in the term:    %s" (show . map prettyStr $ varsFreeInTerm)
    trace $ printf "Variables bound in the context: %s" (show . map prettyStr $ boundVarsInContext)
    let v :: Variable = "TODO"
    let b = Binder (Just v)
    τ1' <- ΔT.patch τ d1
    RS.withInsertLocalAssum (Binder . Just $ v, τ1') $
      ΔT.InsLam () b <$> repair t τ d2

  ΔT.PermutPis p d1      -> do
    (pis, τrest) <- ΔT.extractSomePis (length p) τ
    let pis' = permute p pis
    (lams, trest) <- ΔT.extractSomeLams (length p) t -- TODO: catchError and try something else?
    let lams' = permute p lams
    ΔT.PermutLams p <$> repair (ΔT.mkLams lams' trest) (ΔT.mkPis pis' τrest) d1

  ΔT.RemoveApp _ -> genericRepair t τ -- FIXME: improve this

  ΔT.RemoveLam _ -> genericRepair t τ -- FIXME: improve this

  ΔT.RemovePi _ -> genericRepair t τ -- FIXME: improve this
