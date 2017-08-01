{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repair.Benchmark where

import           Control.Lens ((&))
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Control.Monad.Identity
import           Data.Foldable
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.Inductive as DI
import qualified Diff.List as DL
import qualified Diff.Script as DS
import qualified Diff.Term as DT
import qualified Diff.Vernacular as DV
import qualified Inductive.Constructor as C
import qualified Inductive.Inductive as I
import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Repair.Script as RS
import           Repair.State
import qualified Repair.Term as RT
import           Script
import           StandardLibrary
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Variable
import qualified Typing.GlobalEnvironment as GE
import qualified Typing.LocalContext as LC
import           Utils
import           Vernacular

patchProof ::
  Raw.Term Variable -> Raw.Type Variable -> DT.Diff Raw.Raw ->
  Eff '[Trace] (Either String (Raw.Term Variable))
patchProof t τ δτ = runAll repairThenPatch
  where
    runAll =
      runError
      . liftM fst
      . flip runState (RepairState (LC.LocalContext []) DL.Same (GE.GlobalEnvironment []) DL.Same :: RepairState)
    repairThenPatch = RT.repair t τ δτ >>= DT.patch t

patchProofTrace ::
  Raw.Term Variable -> Raw.Type Variable -> DT.Diff Raw.Raw ->
  IO (Either String (Raw.Term Variable))
patchProofTrace t τ δτ = runTrace $ patchProof t τ δτ

patchProofSkipTrace ::
  Raw.Term Variable -> Raw.Type Variable -> DT.Diff Raw.Raw ->
  Either String (Raw.Term Variable)
patchProofSkipTrace t τ δτ = skipTrace $ patchProof t τ δτ

data RepairTermBenchmark = RepairTermBenchmark
  { repairTermFromTerm :: Raw.Term Variable
  , repairTermFromType :: Raw.Type Variable
  , repairTermToType   :: Raw.Type Variable
  , repairTermDiff     :: DT.Diff Raw.Raw
  , repairTermExpected :: Raw.Term Variable
  }

repairTermBenchmarks :: [RepairTermBenchmark]
repairTermBenchmarks =

  [ RepairTermBenchmark
    { repairTermFromTerm = unsafeParseRaw "λ b . b"
    , repairTermFromType = unsafeParseRaw "B → B"
    , repairTermToType   = unsafeParseRaw "A → B → B"
    , repairTermDiff     = DT.InsPi () (DT.Change "A") (Binder Nothing) DT.Same
    , repairTermExpected = unsafeParseRaw "λ _ b . b"
    }

  , RepairTermBenchmark
    { repairTermFromTerm = unsafeParseRaw "λ b . b"
    , repairTermFromType = unsafeParseRaw "B → B"
    , repairTermToType   = unsafeParseRaw "B → A → B"
    , repairTermDiff     = DT.CpyPi DT.Same DA.Same (DT.InsPi () (DT.Change "A") (Binder Nothing) DT.Same)
    , repairTermExpected = unsafeParseRaw "λ b _ . b"
    }

  , RepairTermBenchmark
    { repairTermFromTerm = unsafeParseRaw "λ f a . f a"
    , repairTermFromType = unsafeParseRaw "(A → B) → A → B"
    , repairTermToType   = unsafeParseRaw "A → (A → B) → B"
    , repairTermDiff     = DT.PermutPis [1, 0] DT.Same
    , repairTermExpected = unsafeParseRaw "λ a f . f a"
    }

  , RepairTermBenchmark
    { repairTermFromTerm = unsafeParseRaw "λ f a . f a"
    , repairTermFromType = unsafeParseRaw "(A → C) → A → C"
    , repairTermToType   = unsafeParseRaw "(A → B → C) → A → B → C"
    , repairTermDiff     =
      DT.CpyPi
      (DT.CpyPi DT.Same DA.Same (DT.InsPi () (DT.Change "B") (Binder Nothing) DT.Same))
      DA.Same
      (DT.CpyPi DT.Same DA.Same (DT.InsPi () (DT.Change "B") (Binder Nothing) DT.Same))
    , repairTermExpected = unsafeParseRaw "λ f a _ . f a (? @ B)"
    }

  , RepairTermBenchmark
    { repairTermFromTerm = unsafeParseRaw "λ f a c . f a c"
    , repairTermFromType = unsafeParseRaw "(A → C → D) → A → C → D"
    , repairTermToType   = unsafeParseRaw "(A → B → C → D) → A → B → C → D"
    , repairTermDiff     =
      DT.CpyPi
      (DT.CpyPi DT.Same DA.Same (DT.InsPi () (DT.Change "B") (Binder Nothing) (DT.CpyPi DT.Same DA.Same DT.Same)))
      DA.Same
      $ DT.CpyPi DT.Same DA.Same
      $ DT.InsPi () (DT.Change "B") (Binder Nothing)
      $ DT.Same
    , repairTermExpected = unsafeParseRaw "λ f a _ c . f a (? @ B) c"
    }

  ]

repairTermBenchmark :: IO ()
repairTermBenchmark = do
  for_ repairTermBenchmarks $ \ (RepairTermBenchmark fromTerm fromType toType diff expected) -> do
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

repairScript ::
  Script Raw.Raw Variable -> DS.Diff Raw.Raw ->
  Eff '[Trace] (Either String (Script Raw.Raw Variable))
repairScript s δs = runAll repairThenPatch
  where
    runAll =
      runError
      . liftM fst
      . flip runState (RepairState (LC.LocalContext []) DL.Same (GE.GlobalEnvironment []) DL.Same :: RepairState)
    repairThenPatch = do
      δs' <- RS.repair s δs
      trace $ show δs'
      DS.patch s δs'

data RepairScriptBenchmark = RepairScriptBenchmark
  { repairScriptFromScript :: Script  Raw.Raw Variable
  , repairScriptDiff       :: DS.Diff Raw.Raw
  }
  deriving (Show)

repairScriptBenchmarks :: [RepairScriptBenchmark]
repairScriptBenchmarks =
  [ RepairScriptBenchmark
    { repairScriptFromScript = Script
      [ Definition "foo" (unsafeParseRaw "A → B → C → D → A") (unsafeParseRaw "λ a b c d . a")
      , Definition "bar" (unsafeParseRaw "A → B → C → D → A") (unsafeParseRaw "λ a b c d . foo a b c d")
      ]
    , repairScriptDiff =
        DL.Change
        (DV.ChangeDefinition
          DA.Same
          (DT.PermutPis [3, 1, 0, 2] DT.Same)
          DT.Same
        )
        $
        DL.Same
    }

  , RepairScriptBenchmark
    { repairScriptFromScript = Script
      [ Inductive inductiveBool
      , Definition "ifthenelse"
        (unsafeParseRaw "𝔹 → T → T → T" )
        (unsafeParseRaw "λ b t e . t")
      , Definition "negb"
        (unsafeParseRaw "𝔹 → 𝔹")
        (unsafeParseRaw "λ b . ifthenelse b false true")
      ]
    , repairScriptDiff =
      DL.Change
      (DV.ChangeInductive
        (DI.Change
          DA.Same
          DL.Same
          DL.Same
          (DL.Permute [1, 0] DL.Same)
        )
      )
      DL.Same
    }

  ]

repairScriptBenchmark :: IO ()
repairScriptBenchmark = do
  for_ repairScriptBenchmarks $ \ (RepairScriptBenchmark s δs) -> do
    putStrLn $ replicate 80 '='
    putStrLn $ printf "Before:\n%s" (prettyStrU s)
    s' <- case run . runError $ DS.patch s δs of
      Left  (e :: String) -> error "..."
      Right s'            -> return s'
    putStrLn $ printf "Changed:\n%s" (prettyStrU s')
    putStrLn $ printf "Attempting to patch script"
    -- skipTrace (repairScript s δs) & \case
    runTrace (repairScript s δs) >>= \case
      Left  e   -> putStrLn $ printf "Patching failed: %s" e
      Right s'' ->
        putStrLn $ printf "Patching succeeded:\n%s" (prettyStrU s'')
