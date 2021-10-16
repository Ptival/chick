module Goals
  ( Goals (..),
  )
where

import Goal (Goal)
-- import Utils (splitList)

data Goals α ν = Goals
  { focused :: [Goal α ν],
    unfocused :: [([Goal α ν], [Goal α ν])]
  }

-- focus :: Int -> Goals α ν -> Maybe (Goals α ν)
-- focus n (Goals f u) =
--   case splitList n f of
--     Nothing -> Nothing
--     Just (l, x, r) -> Just (Goals [x] ((l, r) : u))
