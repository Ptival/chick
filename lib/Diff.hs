{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language RankNTypes #-}

module Diff where

import Control.Monad.Except
import Data.Generic.Diff
import Data.Maybe
import Text.Megaparsec
import Text.PrettyPrint.Annotated.WL
import Text.Printf

import Parsing.Tactic
import PrettyPrinting.Tactic
import PrettyPrinting.Term
import PrettyPrinting.Utils
import Tactic
--import Term.AlphaEquivalence
import Term.Diff
import Term.Term
--import Term.TypeChecked   as TypeChecked
import Term.Raw                      as Raw
import Typing.TypeChecker
import StandardLibrary

unsafeParseTactic :: String -> Tactic
unsafeParseTactic s = fromJust $ parseMaybe tacticP s

main :: IO ()
main = forM_ patchBenchmark $ \ (τ0, τ1, t0) -> do
  putStrLn $ replicate 60 '-'
  case testPatchTactic τ0 τ1 t0 of
    Left  l -> putStrLn $ printf "Failed: %s" l
    Right t1 -> do
      putStrLn . doc2String $ vcat
        [ text "Success!"
        , prettyTacticDoc t0
        , text "was patched into:"
        , prettyTacticDoc t1
        ]
  return ()

patchBenchmark :: [(Raw.Term, Raw.Term, Tactic)]
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

testPatchTactic :: Raw.Term -> Raw.Term -> Tactic -> Either String Tactic
testPatchTactic τBefore τAfter tactic = do
  let ttype = unsafeParseRaw "Type"
  let checkType t = case typeCheck [] t ttype of
        Left  _  -> Left "Term did not type check"
        Right t' -> Right t'
  concl0 <- checkType τBefore
  concl1 <- checkType τAfter
  case runTactic [] tactic (Goal [] concl0) of
    Left  _ -> throwError "Tactic did not solve the original goal"
    Right _ -> do
      let conclDiff = diff (raw concl0) (raw concl1) :: TermDiff Raw
      case patchTactic concl0 tactic conclDiff of
        Nothing -> throwError "Could not patch the tactic"
        Just tactic1 -> do
          case runTactic [] tactic1 (Goal [] concl1) of
            Left  _ ->
              throwError . printf . doc2String $
              vcat
              [ text "Patched tactic did not solve modified goal"
              , text "Diff:"
              , text $ show conclDiff
              , text "Modified goal:"
              , prettyTermDoc' concl1
              , text "Patched tactic:"
              , prettyTacticDoc tactic1
              ]
            Right _ -> return tactic1

aa, bb, ab :: Raw.Term
aa = unsafeParseRaw "(a : T) → a a"
bb = unsafeParseRaw "(b : T) → b b"
ab = unsafeParseRaw "(b : T) → a b"

{-
τ1, τ2, τ3 :: Raw.Term
τ1 = unsafeParseRaw "T → T"
τ2 = unsafeParseRaw "U → T → T"
τ3 = unsafeParseRaw "T → U → T"

diff12, diff13, diff23 :: TermDiff Raw
diff12 = diff τ1 τ2
diff13 = diff τ1 τ3
diff23 = diff τ2 τ3
-}

{-
patchAtomic :: TermX ξ -> Atomic -> TermDiff ψ -> Maybe Atomic
patchAtomic _τ atomic = \case
  _ -> Just atomic
-}

type TermDiffL ξ = EditScriptL (TermXFamily ξ) (TermX ξ) (TermX ξ)

patchTactic :: TermX ξ -> Tactic -> TermDiff ψ -> Maybe Tactic
patchTactic τ tactic τdiff = patchTacticL τ tactic τdiff

patchTacticL ::
  forall ξ ψ txs tys.
  TermX ξ -> Tactic -> EditScriptL (TermXFamily ψ) txs tys -> Maybe Tactic
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
      forall ψ txs tys. Binder -> EditScriptL (TermXFamily ψ) txs tys -> Maybe Tactic
    insertIntro b diffRest = do
      let nameToIntroduce =
            Binder . Just $
            case unBinder b of
            Nothing -> Variable "TODO"
            Just v  -> v
      tactic' <- patchTacticL τ tactic diffRest
      return $ Semicolon (Atomic (Intro nameToIntroduce)) tactic'
