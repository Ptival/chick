{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Tactic where

import Control.Monad.Except
import GHC.Generics
import Prelude
import Test.QuickCheck
import Text.PrettyPrint.Annotated.WL
import Text.Printf

import DictMetaOut
import Precedence
import PrettyPrinting.LocalContext
import PrettyPrinting.Term
import PrettyPrinting.Variable
import Term.AlphaEquivalence
import Term.AlphaRenaming
import Term.Free
import Term.Term
import Term.TypeChecked              as TypeChecked
import Typing.GlobalEnvironment      as GE
import Typing.LocalContext           as LC

data Goal ξ = Goal
  { hypotheses :: [LocalDeclaration ξ]
  , conclusion :: TermX ξ
  }
  deriving (Generic)

deriving instance (ForallX Eq   ξ) => Eq   (Goal ξ)
deriving instance (ForallX Show ξ) => Show (Goal ξ)

instance (ForallX Arbitrary ξ) => Arbitrary (Goal ξ) where
  arbitrary = Goal <$> take 2 <$> listOf arbitrary <*> arbitrary

prettyGoal :: DictMetaOut a ξ -> PrecedenceTable -> Goal ξ -> Doc a
prettyGoal dict precs (Goal hyps concl) =
  vcat
  [ vcat (map (prettyLocalDeclarationDoc dict precs) hyps)
  , text (replicate 40 '-')
  , prettyTermDoc dict precs concl
  ]

data Goals ξ = Goals
  { focused   :: [Goal ξ]
  , unfocused :: [([Goal ξ], [Goal ξ])]
  }

splitList :: Int -> [a] -> Maybe ([a], a, [a])
splitList n xs =
  revL <$> go n xs
  where
    go 0 (h:t) = Just ([], h, t)
    go m (h:t) = prependL h <$> go (m-1) t
    go _ []    = Nothing
    prependL h (revl, x, r) = (h:revl, x, r)
    revL (l, x, r) = (reverse l, x, r)

focus :: Int -> Goals ξ -> Maybe (Goals ξ)
focus n (Goals f u) =
  case splitList n f of
  Nothing -> Nothing
  Just (l, x, r) -> Just (Goals [x] ((l, r):u))

addHyp ::
  MonadError String m =>
  LocalDeclaration ξ -> [LocalDeclaration ξ] -> m [LocalDeclaration ξ]
addHyp hyp hyps
  | LC.nameOf hyp `elem` map LC.nameOf hyps = throwError "addHyp: name conflict"
  | otherwise = return $ hyp:hyps

data Atomic
  = Exact Variable
  | Intro Binder
  deriving (Show)

runAtomic ::
  MonadError String m =>
  GlobalEnvironment TypeChecked ->
  Atomic -> Goal TypeChecked -> m [Goal TypeChecked]
runAtomic ge a (Goal hyps concl) =
  case a of

    Exact v -> do
      case LC.lookupType v (hyps ++ toLocalContext ge) of
        Nothing ->
          throwError $
          printf "Could not find variable %s in global environment"
          (prettyVariable v)
        Just τ -> do
          if τ `αeq` concl
            then return []
            else throwError $
                 printf "The type of %s does not match the conclusion"
                 (prettyVariable v)

    Intro (Binder mi) ->
      pure <$> -- i.e. returns just the one goal produced
      case concl of
      Let _ (Binder mv) t1 t2 -> runIntro (typeOf t1) t2 (flip LocalDef t1) (mi, mv)
      Pi  _ (Binder mv) τ1 τ2 -> runIntro τ1          τ2 LocalAssum         (mi, mv)
      _ -> throwError "Head constructor does not allow introduction"

  where

    runIntro ::
      MonadError String m =>
      TypeChecked.Term -> TypeChecked.Term ->
      (Variable -> TypeChecked.Term -> LocalDeclaration TypeChecked) ->
      (Maybe Variable, Maybe Variable) -> m (Goal TypeChecked)
    runIntro introed rest h = \case
      (Nothing, Nothing) -> return $ Goal hyps rest
      (Just i, Nothing) -> do
        hyps' <- addHyp (h i introed) hyps
        return $ Goal hyps' rest
      (Nothing, Just v) ->
        if isFree v rest
        then return $ Goal hyps rest
        else throwError "Can't discard a dependent binder"
      (Just i, Just v) ->
        Goal <$> addHyp (h i introed) hyps <*> pure (αrename v i rest)

data Tactic
  = Atomic Atomic
  | Semicolon Tactic Tactic
  deriving (Show)

runTactic ::
  MonadError String m =>
  GlobalEnvironment TypeChecked ->
  Tactic -> Goal TypeChecked -> m [Goal TypeChecked]
runTactic ge t goal =
  case t of
    Atomic a -> runAtomic ge a goal
    Semicolon t1 t2 -> do
      gs <- runTactic ge t1 goal
      gs' <- concat <$> sequence (map (runTactic ge t2) gs)
      return gs'
