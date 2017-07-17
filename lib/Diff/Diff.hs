{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Diff where

--import           Bound.Scope
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Data.Foldable
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.LocalContext as DLC
import qualified Diff.LocalDeclaration as DLD
import qualified Diff.List as DL
import qualified Diff.State as DS
import qualified Diff.Term as DT
import           Diff.Utils
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Binder
import           Term.Term
import qualified Term.Raw as Raw
import           Term.Variable
import           Typing.LocalContext
import           Typing.LocalDeclaration
import           StandardLibrary
import           Utils

-- | `withState` localizes a modification of the state to a given effectful computation
withState ::
  Member (State s) r =>
  (s -> s) -> Eff r a -> Eff r a
withState f e = do
  s <- get
  put (f s)
  r <- e
  put s
  return r

-- | For example:
-- | τ  = A → C → D                             t  = (f a) c
-- | τ' = A → (B → (C -> D))                    t' = ((f a) _) c
-- | Δ  = CpyP Same (InsP B (CpyP Same Same))   δ  = CpyA (InsA (CpyA Same Same) InsH) Same
-- |      ^^^^                                                   ^^^^
-- |                 ^^^^                                  ^^^^
-- |                         ^^^^                    ^^^^

-- | `updateArgs τ δτ`
-- | `τ`  is the type of the remaining Pi-telescope
-- | `δτ` is the diff for `τ` to become the new telescope
updateArgs ::
  ( Member (Exc String) r
  , Member Trace r
  ) =>
  Raw.Term Variable -> DT.Diff Raw.Raw -> Eff r (DT.Diff Raw.Raw)
updateArgs = go DT.Same

  where
    go acc τ δτ =

      -- (do
      --   trace $ printf "go: %s" (prettyStrU τ)
      --   τ' <- DT.patch τ δτ
      --   trace $ printf "δτ: %s" (prettyStrU τ')
      -- )
      -- >>

      case δτ of
      DT.Same -> return acc
      DT.CpyPi _ _ d2    -> do
        (_, _, _, τ2) <- DT.extractPi τ
        go (DT.CpyApp acc DT.Same) τ2 d2
      DT.InsPi _ d1 _ d2 -> do
        τ2 <- DT.patch τ d2
        go (DT.InsApp () acc (hole d1)) τ2 d2
      _ -> throwExc "updateArgs: TODO"
    hole = \case
      DT.Change τ' -> DT.Change (Annot () (Hole ()) τ')
      _            -> DT.Change (Hole ())

-- | `diffProof t τ δτ` assumes `t` is a term whose type is `τ` and `δτ` is a diff describing
-- | how `τ` changed.  It attempts to build a patch `δt` s.t. `patch t δt` has type `patch τ δτ`.
diffProof ::
  ( Member (Exc String) r
  , Member Trace r
  , Member (State DS.State) r
  ) =>
  Raw.Term Variable -> Raw.Type Variable -> DT.Diff Raw.Raw -> Eff r (DT.Diff Raw.Raw)
diffProof t τ δτ =

  -- (do
  --     s <- get
  --     let γ = view DS.context s
  --     trace $ printf "CONTEXT BEF:\n%s" (prettyStrU γ)
  --     γ' <- DLC.patch γ (view DS.contextDiff s)
  --     trace $ printf "CONTEXT AFT:\n%s" (prettyStrU γ')
  -- ) >>

  case δτ of

  -- even though the type has not changed, the term might still need updating to deal with the
  -- changes in the context
  DT.Same                ->
    case t of
      -- f x y z   is   (((f x) y) z)
      App _ _ _ -> do
        -- (f, [x, y, z])
        (fun, _args)   <- DT.extractApps t
        case fun of

          Var _ v -> do
            s <- get
            let γ  = view DS.context s
            let δγ = view DS.contextDiff s
            γ' <- DLC.patch γ δγ
            τv <- case lookupType v γ of
              Nothing ->
                throwExc $ printf
                  "Could not find the type of the function in the old context: %s"
                  (show γ)
              Just τv -> return τv
            _τv' <- case lookupType v γ' of
              Nothing  -> throwExc "Could not find the type of the function in the new context"
              Just τv' -> return τv'
            δτv <- DLC.findLocalDeclarationDiff v γ δγ
            -- trace $ printf "About to update args with: %s" (show δτ)
            updateArgs τv δτv

          _ -> throwExc "diffProof, Same, App, Not Var"

      -- even though the diff is same, something inside might need updating
      Lam _ bt -> do
        let (b, tlam) = unscopeTerm bt
        (_, τ1, _, τ2) <- DT.extractPi τ
        withState
          (   over DS.context     (addLocalAssum (b, τ1))
          >>> over DS.contextDiff (DL.Keep)
          ) $ do
          DT.CpyLam DA.Same <$> diffProof tlam τ2 DT.Same

      _ -> do
        -- s :: DS.State <- get
        -- trace $ printf "SAME:        %s" (prettyStrU t)
        -- let γ = view DS.context s
        -- trace $ printf "CONTEXT BEF:\n%s" (prettyStrU γ)
        -- γ' <- DLC.patch γ (view DS.contextDiff s)
        -- trace $ printf "CONTEXT AFT:\n%s" (prettyStrU γ')
        return DT.Same

  DT.Change τ'      -> return $ DT.Change $ Annot () (Hole ()) τ'

  DT.CpyApp _ _     -> throwExc "diffProof: CpyApp"
  DT.CpyLam _ _     -> throwExc "diffProof: CpyLam"
  DT.InsApp _ _ _   -> throwExc "diffProof: InsApp"
  DT.InsLam _ _ _   -> throwExc "diffProof: InsLam"
  DT.PermutLams _ _ -> throwExc "diffProof: PermutLams"

  DT.CpyPi d1 DA.Same d2 ->
    case (t, τ) of
      (Lam _ bt, Pi _ τ1 bτ2) -> do
        let (b, _) = unscopeTerm bt
        withState
          (   over DS.context     (addLocalAssum (b, τ1))
          >>> over DS.contextDiff (DL.Change (DLD.Change DA.Same d1))
          ) $ do
          DT.CpyLam DA.Same <$> diffProof (snd $ unscopeTerm bt) (snd $ unscopeTerm bτ2) d2
      _ -> throwExc "diffProof: CpyPi Same"

  DT.CpyPi _ _ _         -> throwExc "diffProof: CpyPi"

  DT.InsPi _ d1 _b d2      -> do
    -- I think what we want here is:
    -- - find a b' binder like b that is free in t
    -- - InsLam b'
    -- - recursively diff by substituting b' for b
    s <- get
    let varsFreeInTerm = foldr (\ v -> (v :)) [] t
    let varsBoundInContext = boundNames (view DS.context s)
    trace $ printf "Variables free  in the term:    %s" (show . map prettyStr $ varsFreeInTerm)
    trace $ printf "Variables bound in the context: %s" (show . map prettyStr $ varsBoundInContext)
    let v :: Variable = "todo"
    let b = Binder (Just v)
    τ1' <- DT.patch τ d1
    withState
      (   over DS.context     id
      >>> over DS.contextDiff (DL.Insert (LocalAssum v τ1'))
      ) $ DT.InsLam () b <$> diffProof t τ d2

  DT.PermutPis p d1      -> do
    (pis, τrest) <- DT.extractSomePis (length p) τ
    let pis' = permute p pis
    (lams, trest) <- DT.extractSomeLams (length p) t -- TODO: catchError and try something else?
    let lams' = permute p lams
    DT.PermutLams p <$> diffProof (DT.mkLams lams' trest) (DT.mkPis pis' τrest) d1

patchProof ::
  Raw.Term Variable -> Raw.Type Variable -> DT.Diff Raw.Raw ->
  Eff '[Trace] (Either String (Raw.Term Variable))
patchProof t τ δτ = runAll diffProofThenPatch
  where
    runAll =
      runError
      . liftM fst
      . flip runState (DS.State (LocalContext []) DL.Same)
    diffProofThenPatch = diffProof t τ δτ >>= DT.patch t

patchProofTrace ::
  Raw.Term Variable -> Raw.Type Variable -> DT.Diff Raw.Raw ->
  IO (Either String (Raw.Term Variable))
patchProofTrace t τ δτ = runTrace $ patchProof t τ δτ

patchProofSkipTrace ::
  Raw.Term Variable -> Raw.Type Variable -> DT.Diff Raw.Raw ->
  Either String (Raw.Term Variable)
patchProofSkipTrace t τ δτ = skipTrace $ patchProof t τ δτ

data PatchBenchmark = PatchBenchmark
  { patchFromTerm :: Raw.Term Variable
  , patchFromType :: Raw.Type Variable
  , patchToType   :: Raw.Type Variable
  , patchDiff     :: DT.Diff Raw.Raw
  , patchExpected :: Raw.Term Variable
  }

patchBenchmark :: [PatchBenchmark]
patchBenchmark =

  [ PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ b . b"
    , patchFromType = unsafeParseRaw "B → B"
    , patchToType   = unsafeParseRaw "A → B → B"
    , patchDiff     = DT.InsPi () (DT.Change "A") (Binder Nothing) DT.Same
    , patchExpected = unsafeParseRaw "λ _ b . b"
    }

  , PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ b . b"
    , patchFromType = unsafeParseRaw "B → B"
    , patchToType   = unsafeParseRaw "B → A → B"
    , patchDiff     = DT.CpyPi DT.Same DA.Same (DT.InsPi () (DT.Change "A") (Binder Nothing) DT.Same)
    , patchExpected = unsafeParseRaw "λ b _ . b"
    }

  , PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ f a . f a"
    , patchFromType = unsafeParseRaw "(A → B) → A → B"
    , patchToType   = unsafeParseRaw "A → (A → B) → B"
    , patchDiff     = DT.PermutPis [1, 0] DT.Same
    , patchExpected = unsafeParseRaw "λ a f . f a"
    }

  , PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ f a . f a"
    , patchFromType = unsafeParseRaw "(A → C) → A → C"
    , patchToType   = unsafeParseRaw "(A → B → C) → A → B → C"
    , patchDiff     =
      DT.CpyPi
      (DT.CpyPi DT.Same DA.Same (DT.InsPi () (DT.Change "B") (Binder Nothing) DT.Same))
      DA.Same
      (DT.CpyPi DT.Same DA.Same (DT.InsPi () (DT.Change "B") (Binder Nothing) DT.Same))
    , patchExpected = unsafeParseRaw "λ f a _ . f a (? @ B)"
    }

  , PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ f a c . f a c"
    , patchFromType = unsafeParseRaw "(A → C → D) → A → C → D"
    , patchToType   = unsafeParseRaw "(A → B → C → D) → A → B → C → D"
    , patchDiff     =
      DT.CpyPi
      (DT.CpyPi DT.Same DA.Same (DT.InsPi () (DT.Change "B") (Binder Nothing) (DT.CpyPi DT.Same DA.Same DT.Same)))
      DA.Same
      $ DT.CpyPi DT.Same DA.Same
      $ DT.InsPi () (DT.Change "B") (Binder Nothing)
      $ DT.Same
    , patchExpected = unsafeParseRaw "λ f a _ c . f a (? @ B) c"
    }

  ]

benchmark :: IO ()
benchmark = do
  for_ patchBenchmark $ \ (PatchBenchmark fromTerm fromType toType diff expected) -> do
    putStrLn $ printf "Attempting to patch `%s` assumed to have type `%s` to type `%s`" (prettyStrU fromTerm) (prettyStrU fromType) (prettyStrU toType)
    let diffed = run . runError $ DT.patch fromType diff
    if diffed == (Right toType :: Either String (Raw.Type Variable))
      then
      runTrace (patchProof fromTerm fromType diff) >>= \case
      Left  e -> putStrLn $ printf "Patching failed: %s" e
      Right r ->
        if r == expected
        then putStrLn $ printf "Patching succeeded: %s" (prettyStrU r)
        else do
          putStrLn $ printf "Patching succeeded, but did not match expectations"
          putStrLn $ printf "Expected: %s" (prettyStrU expected)
          putStrLn $ printf "Obtained: %s" (prettyStrU r)
      else do
        putStrLn $ printf "Sanity-checking diff %s failed" (show diff)
        case diffed of
          Left  e -> do
            putStrLn $ printf "Diffing failed: %s" e
          Right d -> do
            putStrLn $ printf "Original type: %s" (prettyStrU toType)
            putStrLn $ printf "Diffed   type: %s" (prettyStrU d)
