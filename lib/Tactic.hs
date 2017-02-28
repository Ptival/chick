module Tactic where

import Term

data Declaration ξ
  = LocalAssum Binder (TypeX ξ)
  | LocalDef   Binder (TypeX ξ) (TermX ξ)

data Goal ξ = Goal
  { hyps  :: [Declaration ξ]
  , concl :: TermX ξ
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

--runAtomic :: Atomic -> Goals ξ -> Maybe (Goals ξ)
