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
import Data.Default
import GHC.Generics
import Prelude
import Test.QuickCheck
import Text.PrettyPrint.Annotated.WL

import DictMetaOut
import Precedence
import PrettyPrinting
import Term.AlphaRenaming
import Term.Free
import Term.Term
import Term.TypeChecked              as TypeChecked
import Typing.LocalContext

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
  [ vcat (map (prettyLocalDeclaration dict precs) hyps)
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

data Atomic
  = Intro Binder

addHyp ::
  MonadError String m =>
  LocalDeclaration ξ -> [LocalDeclaration ξ] -> m [LocalDeclaration ξ]
addHyp hyp hyps
  | nameOf hyp `elem` map nameOf hyps = throwError "addHyp: name conflict"
  | otherwise = return $ hyp:hyps

runAtomic ::
  forall m.
  (ForallX Default TypeChecked, MonadError String m) =>
  Atomic -> Goal TypeChecked -> m (Goal TypeChecked)
runAtomic a (Goal hyps concl) =
  case a of

    Intro (Binder mi) ->
      case concl of
      Lam _ (Binder mv) t     -> runIntro (typeOf t)  t  LocalAssum         (mi, mv)
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
      (Just i, Nothing) -> Goal <$> addHyp (h i introed) hyps <*> pure rest
      (Nothing, Just v) ->
        if isFree v rest
        then return $ Goal hyps rest
        else throwError "Can't discard a dependent binder"
      (Just i, Just v) ->
        Goal <$> addHyp (h i introed) hyps <*> pure (αrename v i rest)
