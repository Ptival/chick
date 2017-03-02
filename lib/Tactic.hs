{-# language FlexibleContexts #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}

module Tactic where

import Control.Monad.Fail
import Prelude hiding (fail)

import Term.Term

data Declaration ξ
  = LocalAssum Variable (TypeX ξ)
  | LocalDef   Variable (TypeX ξ) (TermX ξ)

deriving instance ForallX Eq ξ => Eq (Declaration ξ)

nameOf :: Declaration ξ -> Variable
nameOf (LocalAssum v _)   = v
nameOf (LocalDef   v _ _) = v

data Goal ξ = Goal
  { hypotheses :: [Declaration ξ]
  , conclusion :: TermX ξ
  }

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

addHyp :: MonadFail m => Declaration ξ -> [Declaration ξ] -> m [Declaration ξ]
addHyp hyp hyps
  | nameOf hyp `elem` map nameOf hyps = fail "addHyp: name conflict"
  | otherwise = return $ hyp:hyps

-- runAtomic :: MonadFail m => Atomic -> Goal ξ -> m (Goal ξ)
-- runAtomic a (Goal hyps concl) =
--   case a of
--     Intro i ->
--       case concl of
--         Pi _ b τ1 τ2 ->
--           Goal <$> addHyp (LocalAssum s τ1) hyps <*> _
