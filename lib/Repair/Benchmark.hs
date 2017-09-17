{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repair.Benchmark where

-- import           Control.Lens ((&))
import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
-- import           Control.Monad.Identity
import           Data.Foldable
import           Text.Printf

import qualified Diff.Atom as DA
import qualified Diff.List as DL
import qualified Diff.Script as DS
import qualified Diff.Term as DT
import qualified Diff.Vernacular as DV
-- import qualified Inductive.Inductive as I
import           Parsing
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import qualified Repair.Script as RS
import           Repair.State
import qualified Repair.Term as RT
import           Script
import           StandardLibrary
import           StandardLibraryDiff
import           Term.Binder
import qualified Term.Raw as Raw
import           Term.Term
import qualified Typing.GlobalEnvironment as GE
import qualified Typing.LocalContext as LC
import           Utils
import           Vernacular

-- do not use `unsafeParseRaw` anywhere else!
unsafeParseRaw :: String -> Raw.Term Variable
unsafeParseRaw s =
  case parseMaybeTerm s of
    Nothing -> error $ printf "unsafeParseRaw: could not parse %s" s
    Just t  -> t

patchProof ::
  Raw.Term Variable -> Raw.Type Variable -> DT.Diff Raw.Raw ->
  Eff '[Trace] (Either String (Raw.Term Variable))
patchProof t τ δτ = runAll repairThenPatch
  where
    runAll =
      runError
      . liftM fst
      . flip runState initialRepairState
    initialRepairState =
      RepairState
      (LC.LocalContext [])
      DL.Same
      (GE.GlobalEnvironment [])
      DL.Same
    repairThenPatch =
      RT.repair t τ δτ
      >>=
      (\ δ -> do
          trace $ printf "REPAIR COMPLETED: %s" $ prettyStr δ
          return δ
      )
      >>= DT.patch t

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

termBench1, termBench2, termBench3, termBench4, termBench5 :: RepairTermBenchmark

termBench1 =
  RepairTermBenchmark
  { repairTermFromTerm = unsafeParseRaw "λ b . b"
  , repairTermFromType = unsafeParseRaw "B → B"
  , repairTermToType   = unsafeParseRaw "A → B → B"
  , repairTermDiff     =
    DT.InsPi () (DT.Replace "A") (Binder Nothing)
    $ DT.Same
  , repairTermExpected = unsafeParseRaw "λ _ b . b"
  }

termBench2 =
  RepairTermBenchmark
  { repairTermFromTerm = unsafeParseRaw "λ b . b"
  , repairTermFromType = unsafeParseRaw "B → B"
  , repairTermToType   = unsafeParseRaw "B → A → B"
  , repairTermDiff     =
    DT.CpyPi DT.Same DA.Same
    $ DT.InsPi () (DT.Replace "A") (Binder Nothing)
    $ DT.Same
  , repairTermExpected = unsafeParseRaw "λ b _ . b"
  }

termBench3 =
  RepairTermBenchmark
  { repairTermFromTerm = unsafeParseRaw "λ f a . f a"
  , repairTermFromType = unsafeParseRaw "(A → B) → A → B"
  , repairTermToType   = unsafeParseRaw "A → (A → B) → B"
  , repairTermDiff     =
    DT.PermutPis [1, 0]
    $ DT.Same
  , repairTermExpected = unsafeParseRaw "λ a f . f a"
  }

termBench4 =
  RepairTermBenchmark
  { repairTermFromTerm = unsafeParseRaw "λ f a . f a"
  , repairTermFromType = unsafeParseRaw "(A → C) → A → C"
  , repairTermToType   = unsafeParseRaw "(A → B → C) → A → B → C"
  , repairTermDiff     =
      DT.CpyPi
      (DT.CpyPi DT.Same DA.Same
       (DT.InsPi () (DT.Replace "B") (Binder Nothing) DT.Same)
      )
      DA.Same
      (DT.CpyPi DT.Same DA.Same
       (DT.InsPi () (DT.Replace "B") (Binder Nothing) DT.Same)
      )
  , repairTermExpected = unsafeParseRaw "λ f a _ . f a (? @ B)"
  }

termBench5 =
  RepairTermBenchmark
  { repairTermFromTerm = unsafeParseRaw "λ f a c . f a c"
  , repairTermFromType = unsafeParseRaw "(A → C → D) → A → C → D"
  , repairTermToType   = unsafeParseRaw "(A → B → C → D) → A → B → C → D"
  , repairTermDiff     =
      DT.CpyPi
      ( DT.CpyPi DT.Same DA.Same
      $ DT.InsPi () (DT.Replace "B") (Binder Nothing)
      $ DT.CpyPi DT.Same DA.Same DT.Same
      )
      DA.Same
      ( DT.CpyPi DT.Same DA.Same
      $ DT.InsPi () (DT.Replace "B") (Binder Nothing)
      $ DT.Same
      )
  , repairTermExpected = unsafeParseRaw "λ f a _ c . f a (? @ B) c"
  }

repairTermBenchmarks :: [RepairTermBenchmark]
repairTermBenchmarks = []
  ++ [termBench1]
  ++ [termBench2]
  ++ [termBench3]
  ++ [termBench4]
  ++ [termBench5]

repairTermBenchmark :: IO ()
repairTermBenchmark = do
  for_ repairTermBenchmarks $ \
    (RepairTermBenchmark fromTerm fromType toType diff expected) -> do
    putStrLn $
      printf "Attempting to patch `%s` assumed to have type `%s` to type `%s`"
      (prettyStrU fromTerm) (prettyStrU fromType) (prettyStrU toType)
    diffed <- runSkipTrace . runError $ DT.patch fromType diff
    if diffed == (Right toType :: Either String (Raw.Type Variable))
      then
      runSkipTrace (patchProof fromTerm fromType diff) >>= \case
      -- runTrace (patchProof fromTerm fromType diff) >>= \case
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
      . flip runState initialRepairState
    initialRepairState =
      RepairState (LC.LocalContext []) DL.Same (GE.GlobalEnvironment []) DL.Same
    repairThenPatch = do
      δs' <- RS.repair s δs
      trace $ printf "COMPUTED PATCH:\n\n%s\n\n" (show δs')
      DS.patch s δs'

data RepairScriptBenchmark = RepairScriptBenchmark
  { repairScriptFromScript :: Script Raw.Raw Variable
  , repairScriptDiff       :: DS.Diff Raw.Raw
  , repairScriptExpected   :: Maybe (Script Raw.Raw Variable)
  }
  deriving (Show)

repairFlippedArguments :: RepairScriptBenchmark
repairFlippedArguments = RepairScriptBenchmark

  { repairScriptFromScript = Script
    [ Definition "foo"
      (unsafeParseRaw "A → B → C → D → A")
      (unsafeParseRaw "λ a b c d . a")
    , Definition "bar"
      (unsafeParseRaw "A → B → C → D → A")
      (unsafeParseRaw "λ a b c d . foo a b c d")
    ]

  , repairScriptDiff =
      DL.Modify
      (DV.ModifyDefinition
        DA.Same
        (DT.PermutPis [3, 1, 0, 2] DT.Same)
        DT.Same
      )
      $
      DL.Same

  , repairScriptExpected = Nothing
  }

repairListToVec :: RepairScriptBenchmark
repairListToVec = RepairScriptBenchmark

  { repairScriptFromScript = Script
    [ Inductive indList
    , Definition "list1"
      (unsafeParseRaw "List ℕ")
      (unsafeParseRaw "cons zero nil")
    , Definition "length"
      (unsafeParseRaw "(T : Type) → List T → ℕ")
      (unsafeParseRaw
       "λ T l . List_rec T (λ _ . ℕ) O (λ _ _ lt . S lt) l")
    ]

  , repairScriptDiff =
    DL.Modify (DV.ModifyInductive δListToVec)
    $ DL.Same

  , repairScriptExpected = Just $ Script
    [ Inductive indVec
    , Definition "list1"
      (unsafeParseRaw "Vec ℕ (? @ ℕ)")
      (unsafeParseRaw "vcons zero (? @ ℕ) nil")
    ]

  }

repairScriptBenchmarks :: [RepairScriptBenchmark]
repairScriptBenchmarks =
  []
  -- ++ [repairFlippedArguments]
  ++ [repairListToVec]

repairScriptBenchmark :: IO ()
repairScriptBenchmark = do
  putStrLn $ replicate 100 '\n'
  for_ repairScriptBenchmarks $ \ (RepairScriptBenchmark s δs me) -> do
    putStrLn $ "(*" ++ replicate 70 '*' ++ "*)"
    putStrLn "\n(*** Before: ***)\n"
    printScript s
    result <- runTrace . runError $ DS.patch s δs
    s' <- case result of
      Left  (_e :: String) -> error "..."
      Right s'             -> return s'
    putStrLn "\n(*** Modified: ***)\n"
    printScript s'
    putStrLn $ printf "\n(*** Attempting to patch script ***)\n"
    -- runSkipTrace (repairScript s δs) >>= \case
    runTrace (repairScript s δs) >>= \case
      Left  e   -> putStrLn $ printf "Patching failed: %s" e
      Right s'' -> case me of
        Nothing -> do
          putStrLn "\n(*** Patching succeeded: ***)\n"
          printScript s''
        Just e -> do
          if s'' == e
            then do
            putStrLn "\n(*** Patching succeeded, and matched expectations: ***)\n"
            printScript s''
            else do
            putStrLn "\n(*** Patching succeeded, BUT did not match expectations: ***)\n"
            putStrLn "(*** Expected ***)\n"
            printScript e
            putStrLn $ show e
            putStrLn "(*** Obtained ***)\n"
            printScript s''
            putStrLn $ show s''
  where
    printScript (Script s) = forM_ s $ putStrLn . prettyStr
