{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repair.Benchmark where

import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Data.Foldable
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.State as DS
import qualified Diff.Term as DT
import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Repair.Term as RT
import           StandardLibrary
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Variable
import qualified Typing.GlobalEnvironment as GE
import qualified Typing.LocalContext as LC
import           Utils

patchProof ::
  Raw.Term Variable -> Raw.Type Variable -> DT.Diff Raw.Raw ->
  Eff '[Trace] (Either String (Raw.Term Variable))
patchProof t τ δτ = runAll repairThenPatch
  where
    runAll =
      runError
      . liftM fst
      . flip runState (DS.State (LC.LocalContext []) DL.Same (GE.GlobalEnvironment []) DL.Same :: DS.State)
    repairThenPatch = RT.repair t τ δτ >>= DT.patch t

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
