{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Diff.Diff where

import           Bound.Scope
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

-- | `diffProof t (τ, d)` assumes `t` is a term whose type is `τ` and `d` is a diff describing
-- | how `τ` changed.  It attempts to build a patch `p` s.t. `patch t p` has type `patch τ d`.
diffProof ::
  ( Member (Exc String)               r
  , Member Trace                      r
  , Member (State DS.State) r
  ) =>
  Raw.Term Variable -> (Raw.Type Variable, DT.Diff Raw.Raw) -> Eff r (DT.Diff Raw.Raw)
diffProof t (τ, d) = case d of

  DT.Same                ->
    case t of
      -- f x y z   is   (((f x) y) z)
      App _ _ _ -> do
        (fun, args)   <- DT.extractApps t
        case fun of
          Var _ v -> do
            s <- get
            let γ = view DS.context s
            γ' <- DLC.patch γ (view DS.contextDiff s)
            τv <- case lookupType v γ of
              Nothing -> throwExc "Could not find the type of the function in the old context"
              Just τv -> return τv
            τv' <- case lookupType v γ' of
              Nothing  -> throwExc "Could not find the type of the function in the new context"
              Just τv' -> return τv'
            (τargs, rest) <- DT.extractPis (length args) τv
            trace $ printf "The function being applied is: %s" (prettyStrU fun)
            trace $ printf "Args: %s" (show (map (prettyStrU . snd) args))
            trace $ printf "Args: %s" (show (map (prettyStrU . view _3) τargs))
            trace $ printf "Type bef: %s" (prettyStrU τv)
            trace $ printf "Type aft: %s" (prettyStrU τv')
            -- trace $ printf "App, t1: %s" (prettyStrU t1)
            -- trace $ printf "App, t2: %s" (prettyStrU t2)
            -- trace $ printf "App, old type: %s" (prettyStrU τ)
            -- τ' <- DT.patch τ d
            -- trace $ printf "App, new type: %s" (prettyStrU τ')
            return DT.Same
          _ -> throwExc "diffProof, Same, App, Not Var"
      _ -> do
        s <- get
        trace $ printf "SAME:        %s" (prettyStrU t)
        let γ = view DS.context s
        trace $ printf "CONTEXT BEF:\n%s" (prettyStrU γ)
        γ' <- DLC.patch γ (view DS.contextDiff s)
        trace $ printf "CONTEXT AFT:\n%s" (prettyStrU γ')
        return DT.Same

  DT.Change τ'           -> return $ DT.Change $ Annot () (Hole ()) τ'

  DT.CpyLam _ _          -> throwExc "diffProof: CpyLam"

  DT.InsLam _ _ _        -> throwExc "diffProof: InsLam"

  DT.PermutLams _ _      -> throwExc "diffProof: PermutLams"

  DT.CpyPi d1 DA.Same d2 ->
    case (t, τ) of
      (Lam _ bt, Pi _ τ1 bτ2) -> do
        let (b, t) = unscopeTerm bt
        withState
          (   over DS.context     (addLocalAssum (b, τ1))
          >>> over DS.contextDiff (DL.Change (DLD.Change DA.Same d1))
          ) $ do
          DT.CpyLam DA.Same <$> diffProof (snd $ unscopeTerm bt) (snd $ unscopeTerm bτ2, d2)
      _ -> throwExc "diffProof: CpyPi Same"

  DT.CpyPi _ _ _         -> throwExc "diffProof: CpyPi"

  DT.InsPi _ d1 b d2      -> do
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
      ) $ DT.InsLam () b <$> diffProof t (τ, d2)

  DT.PermutPis p d1      -> do
    (pis, τrest) <- DT.extractPis (length p) τ
    let pis' = DT.permute p pis
    (lams, trest) <- DT.extractLams (length p) t -- TODO: catchError and try something else?
    let lams' = DT.permute p lams
    DT.PermutLams p <$> diffProof (DT.mkLams lams' trest) (DT.mkPis pis' τrest, d1)

patchProof :: Raw.Term Variable -> (Raw.Type Variable, DT.Diff Raw.Raw) -> Eff '[Trace] (Either String (Raw.Term Variable))
patchProof t (τ, d) = runAll diffProofThenPatch
  where
    runAll =
      runError
      . liftM fst
      . flip runState (DS.State (LocalContext []) DL.Same)
    diffProofThenPatch = diffProof t (τ, d) >>= DT.patch t

patchProofTrace :: Raw.Term Variable -> (Raw.Type Variable, DT.Diff Raw.Raw) -> IO (Either String (Raw.Term Variable))
patchProofTrace t (τ, d) = runTrace $ patchProof t (τ, d)

patchProofSkipTrace :: Raw.Term Variable -> (Raw.Type Variable, DT.Diff Raw.Raw) -> Either String (Raw.Term Variable)
patchProofSkipTrace t (τ, d) = skipTrace $ patchProof t (τ, d)

data PatchBenchmark = PatchBenchmark
  { patchFromTerm :: Raw.Term Variable
  , patchFromType :: Raw.Type Variable
  , patchToType   :: Raw.Type Variable
  , patchDiff     :: DT.Diff Raw.Raw
  }

patchBenchmark :: [PatchBenchmark]
patchBenchmark =
  [ PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ b . b"
    , patchFromType = unsafeParseRaw "B → B"
    , patchToType   = unsafeParseRaw "A → B → B"
    , patchDiff     = DT.InsPi () (DT.Change "A") (Binder Nothing) DT.Same
    }
  , PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ b . b"
    , patchFromType = unsafeParseRaw "B → B"
    , patchToType   = unsafeParseRaw "B → A → B"
    , patchDiff     = DT.CpyPi DT.Same DA.Same (DT.InsPi () (DT.Change "A") (Binder Nothing) DT.Same)
    }
  , PatchBenchmark
    { patchFromTerm = unsafeParseRaw "λ f a . f a"
    , patchFromType = unsafeParseRaw "(A → B) → A → B"
    , patchToType   = unsafeParseRaw "A → (A → B) → B"
    , patchDiff     = DT.PermutPis [1, 0] DT.Same
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
    }
  ]

benchmark :: IO ()
benchmark = do
  for_ patchBenchmark $ \ (PatchBenchmark fromTerm fromType toType diff) -> do
    putStrLn $ printf "Attempting to patch `%s` assumed to have type `%s` to type `%s`" (prettyStrU fromTerm) (prettyStrU fromType) (prettyStrU toType)
    let diffed = run . runError $ DT.patch fromType diff
    if diffed == (Right toType :: Either String (Raw.Type Variable))
      then runTrace (patchProof fromTerm (fromType, diff)) >>= \case
             Left  e -> putStrLn $ printf "Patching failed: %s" e
             Right r -> putStrLn $ printf "Patching succeeded: %s" (prettyStrU r)
      else do
        putStrLn $ printf "Sanity-checking diff %s failed" (show diff)
        case diffed of
          Left  e -> do
            putStrLn $ printf "Diffing failed: %s" e
          Right d -> do
            putStrLn $ printf "Original type: %s" (prettyStrU toType)
            putStrLn $ printf "Diffed   type: %s" (prettyStrU d)
