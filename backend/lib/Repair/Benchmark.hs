{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repair.Benchmark where

import           Control.Monad
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Trace
import           Data.Foldable
import           Text.Megaparsec
import           Text.Printf

import qualified Diff.Atom as ΔA
import qualified Diff.List as ΔL
import qualified Diff.Script as ΔS
import qualified Diff.Term as ΔT
import qualified Diff.Vernacular as ΔV
import           Parsing
import           Parsing.Vernacular
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

repairScriptBenchmarks :: [RepairScriptBenchmark]
repairScriptBenchmarks =
  []
  ++ [repairFlippedArguments]
  ++ [repairListToVec]

-- do not use `unsafeParseRaw` anywhere else!
unsafeParseRaw :: String -> Raw.Term Variable
unsafeParseRaw s =
  case parseMaybeTerm s of
    Nothing -> error $ printf "unsafeParseRaw: could not parse %s" s
    Just t  -> t

-- do not use `unsafeParseVernac` anywhere else!
unsafeParseVernac :: [String] -> Vernacular Raw.Raw Variable
unsafeParseVernac ss =
  let s = unlines (ss ++ ["."]) in
  case parseMaybe vernacularP s of
    Nothing -> error $ printf "unsafeParseVernac: could not parse %s" s
    Just t  -> t

patchProof ::
  Raw.Term Variable -> Raw.Type Variable -> ΔT.Diff Raw.Raw ->
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
      ΔL.Same
      (GE.GlobalEnvironment [])
      ΔL.Same
    repairThenPatch =
      RT.repair t τ δτ
      >>=
      (\ δ -> do
          trace $ printf "REPAIR COMPLETED: %s" $ prettyStr δ
          return δ
      )
      >>= ΔT.patch t

patchProofTrace ::
  Raw.Term Variable -> Raw.Type Variable -> ΔT.Diff Raw.Raw ->
  IO (Either String (Raw.Term Variable))
patchProofTrace t τ δτ = runTrace $ patchProof t τ δτ

patchProofSkipTrace ::
  Raw.Term Variable -> Raw.Type Variable -> ΔT.Diff Raw.Raw ->
  Either String (Raw.Term Variable)
patchProofSkipTrace t τ δτ = skipTrace $ patchProof t τ δτ

data RepairTermBenchmark = RepairTermBenchmark
  { repairTermFromTerm :: Raw.Term Variable
  , repairTermFromType :: Raw.Type Variable
  , repairTermToType   :: Raw.Type Variable
  , repairTermDiff     :: ΔT.Diff Raw.Raw
  , repairTermExpected :: Raw.Term Variable
  }

termBench1, termBench2, termBench3, termBench4, termBench5 :: RepairTermBenchmark

termBench1 =
  RepairTermBenchmark
  { repairTermFromTerm = unsafeParseRaw "λ b , b"
  , repairTermFromType = unsafeParseRaw "B → B"
  , repairTermToType   = unsafeParseRaw "A → B → B"
  , repairTermDiff     =
    ΔT.InsPi () (ΔT.Replace "A") (Binder Nothing)
    $ ΔT.Same
  , repairTermExpected = unsafeParseRaw "λ _ b , b"
  }

termBench2 =
  RepairTermBenchmark
  { repairTermFromTerm = unsafeParseRaw "λ b , b"
  , repairTermFromType = unsafeParseRaw "B → B"
  , repairTermToType   = unsafeParseRaw "B → A → B"
  , repairTermDiff     =
    ΔT.CpyPi ΔT.Same ΔA.Same
    $ ΔT.InsPi () (ΔT.Replace "A") (Binder Nothing)
    $ ΔT.Same
  , repairTermExpected = unsafeParseRaw "λ b _ , b"
  }

termBench3 =
  RepairTermBenchmark
  { repairTermFromTerm = unsafeParseRaw "λ f a , f a"
  , repairTermFromType = unsafeParseRaw "(A → B) → A → B"
  , repairTermToType   = unsafeParseRaw "A → (A → B) → B"
  , repairTermDiff     =
    ΔT.PermutPis [1, 0]
    $ ΔT.Same
  , repairTermExpected = unsafeParseRaw "λ a f , f a"
  }

termBench4 =
  RepairTermBenchmark
  { repairTermFromTerm = unsafeParseRaw "λ f a , f a"
  , repairTermFromType = unsafeParseRaw "(A → C) → A → C"
  , repairTermToType   = unsafeParseRaw "(A → B → C) → A → B → C"
  , repairTermDiff     =
      ΔT.CpyPi
      (ΔT.CpyPi ΔT.Same ΔA.Same
       (ΔT.InsPi () (ΔT.Replace "B") (Binder Nothing) ΔT.Same)
      )
      ΔA.Same
      (ΔT.CpyPi ΔT.Same ΔA.Same
       (ΔT.InsPi () (ΔT.Replace "B") (Binder Nothing) ΔT.Same)
      )
  , repairTermExpected = unsafeParseRaw "λ f a _ , f a (_ : B)"
  }

termBench5 =
  RepairTermBenchmark
  { repairTermFromTerm = unsafeParseRaw "λ f a c , f a c"
  , repairTermFromType = unsafeParseRaw "(A → C → D) → A → C → D"
  , repairTermToType   = unsafeParseRaw "(A → B → C → D) → A → B → C → D"
  , repairTermDiff     =
      ΔT.CpyPi
      ( ΔT.CpyPi ΔT.Same ΔA.Same
      $ ΔT.InsPi () (ΔT.Replace "B") (Binder Nothing)
      $ ΔT.CpyPi ΔT.Same ΔA.Same ΔT.Same
      )
      ΔA.Same
      ( ΔT.CpyPi ΔT.Same ΔA.Same
      $ ΔT.InsPi () (ΔT.Replace "B") (Binder Nothing)
      $ ΔT.Same
      )
  , repairTermExpected = unsafeParseRaw "λ f a _ c , f a (_ : B) c"
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
    diffed <- runSkipTrace . runError $ ΔT.patch fromType diff
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
  Script Raw.Raw Variable -> ΔS.Diff Raw.Raw ->
  Eff '[Trace] (Either String (Script Raw.Raw Variable))
repairScript s δs = runAll repairThenPatch
  where
    runAll =
      runError
      . liftM fst
      . flip runState initialRepairState
    initialRepairState =
      RepairState (LC.LocalContext []) ΔL.Same (GE.GlobalEnvironment []) ΔL.Same
    repairThenPatch = do
      δs' <- RS.repair s δs
      trace $ printf "COMPUTED PATCH:\n\n%s\n\n" (show δs')
      ΔS.patch s δs'

data RepairScriptBenchmark = RepairScriptBenchmark
  { repairScriptFromScript :: Script Raw.Raw Variable
  , repairScriptDiff       :: ΔS.Diff Raw.Raw
  , repairScriptExpected   :: Maybe (Script Raw.Raw Variable)
  }
  deriving (Show)

repairFlippedArguments :: RepairScriptBenchmark
repairFlippedArguments = RepairScriptBenchmark

  { repairScriptFromScript = Script
    [ unsafeParseVernac
      [ "Definition foo : A → B → C → D → A :="
      , "λ a b c d , a"
      ]
    , unsafeParseVernac
      [ "Definition bar : A → B → C → D → A :="
      , "λ a b c d , foo a b c d"
      ]
    ]

  , repairScriptDiff =
      ΔL.Modify
      (ΔV.ModifyDefinition
        ΔA.Same
        ΔA.Same
        (ΔT.PermutPis [3, 1, 0, 2] ΔT.Same)
        ΔT.Same
      )
      $
      ΔL.Same

  , repairScriptExpected = Nothing

  }

repairListToVec :: RepairScriptBenchmark
repairListToVec = RepairScriptBenchmark

  { repairScriptFromScript = Script $
  [ Inductive indEq
  , Inductive indFalse
  , Inductive indOr
  , Inductive indNat
  , Inductive indList
  ]
  ++ map unsafeParseVernac
  [ [ "Definition a_list : list nat :="
    , "cons nat O (nil nat)"
    ]
  , [ "Definition length : ∀ (T : Type), list T → nat :="
    , "λ T l , list_rect T (λ _ , nat) O (λ _ _ lt , S lt) l"
    ]
  , [ "Definition length2 : ∀ (T : Type), list T → nat :="
    , "λ _ l , match l with"
    , "        | nil  _     => O"
    , "        | cons _ h t => S O"
    , "        end"
    ]
  , [ "Definition hd : ∀ (A : Type), A → list A → A :="
    , "λ A default l, list_rect A (λ _, A) default (λ x _ _, x) l"
    ]
  , [ "Definition tl : ∀ (A : Type), list A → list A :="
    , "λ A l, list_rect A (λ _, list A) (nil A) (λ _ x _, x) l"
    ]
  , [ "Definition In : ∀ (A : Type), A → list A → Type :="
    , "λ A a l, list_rect A (λ _, Type) False (λ _ b m, or (eq A b a) (In A a m))"
    ]
  , [ "Fixpoint map : ∀ (A : Type) (B : Type) (f : A → B), list A → Type :="
    , "λ A B f l, match l with"
    , "           | nil _      => nil B"
    , "           | cons _ h t => cons B (f h) (map A B f t)"
    , "           end"
    ]
  ]

  , repairScriptDiff =
    ΔL.nKeeps 4
    $ ΔL.Modify (ΔV.ModifyInductive δListToVec)
    $ ΔL.Same

  , repairScriptExpected = Just $ Script $
    [ Inductive indNat
    , Inductive indOr
    , Inductive indVec
    ]
    ++ map unsafeParseVernac
    [ [ "Definition list1 : Vec nat (_ : nat) :="
      , "vcons nat O (_ : nat) (vnil nat)"
      ]
    , [ "Definition length : ∀ (T : Type), Vec T (_ : nat) → nat :="
      , "λ T l , Vec_rect T (λ _ _ , nat) O (λ _ _ _ lt , S lt) (_ : nat) l"
      ]
    , [ "Definition hd : ∀ (A : Type), A → Vec A (_ : nat) → A :="
      , "λ A default l, Vec_rect A (λ _ _, A) default (λ x _ _ _, x) (_ : nat) l"
      ]
    , [ "Definition tl : ∀ (A : Type), Vec A (_ : nat) → Vec A (_ : nat) :="
      , "λ A l, Vec_rect A (λ _ _, Vec A (_ : nat)) (vnil A) (λ _ _ x _, x) (_ : nat) l"
      ]
    ]

  }

repairScriptBenchmark :: IO ()
repairScriptBenchmark = do
  putStrLn $ replicate 100 '\n'
  for_ repairScriptBenchmarks $ \ (RepairScriptBenchmark s δs me) -> do
    putStrLn $ "(*" ++ replicate 70 '*' ++ "*)"
    putStrLn "\n(*** Before: ***)\n"
    printScript s
    result <- runTrace . runError $ ΔS.patch s δs
    s' <- case result of
      Left  (e :: String) ->
        error $ printf "Could not patch the original script:\n%s" e
      Right s'             -> return s'
    putStrLn "\n(*** Modified: ***)\n"
    printScript s'
    putStrLn $ printf "\n(*** Attempting to patch script ***)\n"
    runSkipTrace (repairScript s δs) >>= \case
    -- runTrace (repairScript s δs) >>= \case
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
    printScript :: Script α Variable -> IO ()
    printScript = putStrLn . prettyStrU