{-# language FlexibleContexts #-}
{-# language MonoLocalBinds #-}

module Diff.Utils
 ( permute
 , throwExc
 ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Text.Printf

-- | Dumbest way to implement this
permute :: [Int] -> [a] -> [a]
permute []      _ = []
permute p@(h : t) l =
  if h >= length l
  then error $ printf
       "Diff.Utils/permute: permutation (%s) is longer than list (%s)"
       (show (length p)) (show (length l))
  else (l !! h) : permute t l

-- | In the presence of overloaded strings, throwError freaks out
throwExc :: Member (Exc String) r => String -> Eff r a
throwExc = throwError
