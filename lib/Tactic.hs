{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Tactic where

import Control.Monad.Except
import GHC.Generics
import Prelude
import Test.QuickCheck
import Text.PrettyPrint.Annotated.WL

import Precedence
import PrettyPrinting
import Term.AlphaRenaming
import Term.Free
import Term.Term

data Declaration ξ
  = LocalAssum Variable (TypeX ξ)
  | LocalDef   Variable (TypeX ξ) (TermX ξ)

deriving instance (ForallX Eq   ξ) => Eq   (Declaration ξ)
deriving instance (ForallX Show ξ) => Show (Declaration ξ)

instance (ForallX Arbitrary ξ) => Arbitrary (Declaration ξ) where
  arbitrary =
    oneof
    [ LocalAssum <$> arbitraryVariable <*> genTerm 2
    , LocalDef   <$> arbitraryVariable <*> genTerm 2 <*> genTerm 2
    ]

prettyDeclaration :: ForallX ((~) a) ξ => PrecedenceTable -> Declaration ξ -> Doc a
prettyDeclaration precs (LocalAssum v τ) =
  sep [text v, char ':', prettyTerm precs τ]
prettyDeclaration precs (LocalDef v t τ) =
  sep [text v, text ":=", prettyTerm precs t, char ':', prettyTerm precs τ]

nameOf :: Declaration ξ -> Variable
nameOf (LocalAssum v _)   = v
nameOf (LocalDef   v _ _) = v

data Goal ξ = Goal
  { hypotheses :: [Declaration ξ]
  , conclusion :: TermX ξ
  }
  deriving (Generic)

deriving instance (ForallX Eq   ξ) => Eq   (Goal ξ)
deriving instance (ForallX Show ξ) => Show (Goal ξ)

instance (ForallX Arbitrary ξ) => Arbitrary (Goal ξ) where
  arbitrary = Goal <$> take 2 <$> listOf arbitrary <*> arbitrary

prettyGoal :: ForallX ((~) a) ξ => PrecedenceTable -> Goal ξ -> Doc a
prettyGoal precs (Goal hyps concl) =
  vcat
  [ text "{{{"
  , vcat (map (prettyDeclaration precs) hyps)
  , text (replicate 40 '-')
  , prettyTerm precs concl
  , text "}}}"
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

addHyp :: MonadError String m => Declaration ξ -> [Declaration ξ] -> m [Declaration ξ]
addHyp hyp hyps
  | nameOf hyp `elem` map nameOf hyps = throwError "addHyp: name conflict"
  | otherwise = return $ hyp:hyps

runAtomic :: MonadError String m => Atomic -> Goal ξ -> m (Goal ξ)
runAtomic a (Goal hyps concl) =
  case a of
    Intro (Binder mi) ->
      case concl of
        Pi _ (Binder mv) τ1 τ2 ->
          case (mi, mv) of

          (Nothing, Nothing) -> return $ Goal hyps τ2

          (Just i, Nothing) ->
            Goal
            <$> addHyp (LocalAssum i τ1) hyps
            <*> pure τ2

          (Nothing, Just v) ->
            if isFree v τ2
            then return $ Goal hyps τ2
            else throwError "Can't discard a dependent binder"

          (Just i, Just v) ->
            Goal
            <$> addHyp (LocalAssum i τ1) hyps
            <*> pure (αrename v i τ2)

        _ -> throwError "TODO"
