{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}

module Diff where

--import Control.Monad.Except
--import Control.Monad.Reader
--import Data.Default
import Data.Generic.Diff
import Data.Maybe
import Text.Megaparsec
--import Text.PrettyPrint.Annotated.WL
--import Text.Printf

import Parsing.Tactic
--import PrettyPrinting.PrettyPrintable
--import PrettyPrinting.PrettyPrintableUnannotated
--import PrettyPrinting.Tactic
--import PrettyPrinting.Utils
import Tactic
--import Term.AlphaEquivalence
--import Term.Binder
import Term.Diff
import Term.Term
--import Term.TypeChecked   as TypeChecked
import Term.Raw                      as Raw
import Term.Variable
--import Typing.GlobalEnvironment
--import Typing.LocalContext
--import Typing.TypeChecker
import StandardLibrary

unsafeParseTactic :: String -> Tactic Variable
unsafeParseTactic s = fromJust $ parseMaybe tacticP s

{-
main :: IO ()
main = forM_ patchBenchmark $ \ (τ0, τ1, t0) -> do
  putStrLn $ replicate 60 '-'
  case testPatchTactic τ0 τ1 t0 of
    Left  l -> putStrLn $ printf "Failed: %s" l
    Right t1 -> do
      putStrLn . doc2String $ vcat
        [ text "Success!"
        , prettyDoc t0
        , text "was patched into:"
        , prettyDoc t1
        ]
  return ()
-}

patchBenchmark :: [(Raw.Term Variable, Raw.Term Variable, Tactic Variable)]
patchBenchmark =
  map (\ (τ1, τ2, t) -> (unsafeParseRaw τ1, unsafeParseRaw τ2, unsafeParseTactic t))
  [ ( "(A : Type) → A → A"
    , "(B : Type) → (A : Type) → A → A"
    , "intro A; intro a; exact a"
    )
  , ( "(A : Type) → A → A"
    , "(A : Type) → (B : Type) → A → A"
    , "intro A; intro a; exact a"
    )
  , ( "(A : Type) → A → A"
    , "(A : Type) → A → (B : Type) → A"
    , "intro A; intro a; exact a"
    )
  , ( "(A : Type) → A → A"
    , "(A : Type) → A → A → A"
    , "intro A; intro a; exact a"
    )
  ]

{-
testPatchTactic :: Raw.Term Variable -> Raw.Term Variable -> Tactic Variable -> Either String (Tactic Variable)
testPatchTactic τBefore τAfter tactic = do
  let ttype = unsafeParseRaw "Type"
  let checkType t = case typeCheck (GlobalEnvironment []) t ttype of
        Left  _  -> Left "Term did not type check"
        Right t' -> Right t'
  concl0 <- checkType τBefore
  concl1 <- checkType τAfter
  case runTactic (GlobalEnvironment []) tactic (Goal (LocalContext []) concl0) of
    Left  _ -> throwError "Tactic did not solve the original goal"
    Right _ -> do
      let conclDiff = diff (raw concl0) (raw concl1) :: TermDiff Raw Variable
      case patchTactic concl0 tactic conclDiff of
        Nothing -> throwError "Could not patch the tactic"
        Just tactic1 -> do
          case runTactic (GlobalEnvironment []) tactic1 (Goal (LocalContext []) concl1) of
            Left  _ ->
              throwError . doc2String $
              vcat
              [ text "Patched tactic did not solve modified goal"
              , text "Diff:"
              , text $ show conclDiff
              , text "Modified goal:"
              , runReader (prettyDocU concl1) def
              , text "Patched tactic:"
              , prettyDoc tactic1
              ]
            Right _ -> return tactic1
-}

aa, bb, ab :: Raw.Term Variable
aa = unsafeParseRaw "(a : T) → a a"
bb = unsafeParseRaw "(b : T) → b b"
ab = unsafeParseRaw "(b : T) → a b"

τTT, τUTT, τTUT :: Raw.Term Variable
τTT = unsafeParseRaw "T → T"
τUTT = unsafeParseRaw "U → T → T"
τTUT = unsafeParseRaw "T → U → T"

rawDiff :: Raw.Term Variable -> Raw.Term Variable -> TermDiff Raw
rawDiff = diff

diff12, diff13, diff23 :: TermDiff Raw
diff12 = diff τTT τUTT
diff13 = diff τTT τTUT
diff23 = diff τUTT τTUT

testing :: Raw.Term Variable
testing = patch (rawDiff aa ab) aa

{-
patchAtomic :: TermX ξ -> Atomic -> TermDiff ψ -> Maybe Atomic
patchAtomic _τ atomic = \case
  _ -> Just atomic
-}

type TermDiffL ξ ν = EditScriptL (TermXFamily ξ) (TermX ξ ν) (TermX ξ ν)

{-
patchTactic :: TermX ξ ν -> Tactic ν -> TermDiff ψ ν -> Maybe (Tactic ν)
patchTactic τ tactic τdiff = patchTacticL τ tactic τdiff

patchTacticL ::
  forall ξ ν ψ txs tys.
  TermX ξ ν -> Tactic ν -> EditScriptL (TermXFamily ψ ν) txs tys -> Maybe (Tactic ν)
patchTacticL τ tactic τdiff =

  case (τdiff, τ) of

  -- a Pi was added
  (Cpy Pi' (Ins (Binder' b) (Ins _ diffRest)), _) ->
    insertIntro b diffRest
  (Ins Pi' (Ins (Binder' b) (Ins _ diffRest)), _) ->
    insertIntro b diffRest

  _ -> Just tactic

  where

    insertIntro ::
      forall ν ψ txs tys. Binder ν -> EditScriptL (TermXFamily ψ ν) txs tys -> Maybe (Tactic ν)
    insertIntro b diffRest = do
      let nameToIntroduce =
            Binder . Just $
            case unBinder b of
            Nothing -> Variable "TODO"
            Just v  -> v
      tactic' <- patchTacticL τ tactic diffRest
      return $ Semicolon (Atomic (Intro nameToIntroduce)) tactic'
-}
