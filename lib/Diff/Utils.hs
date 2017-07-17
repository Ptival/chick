{-# language FlexibleContexts #-}

module Diff.Utils
 ( permute
 , throwExc
 ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception

-- | Dumbest way to implement this
permute :: [Int] -> [a] -> [a]
permute []      _ = []
permute (h : t) l = (l !! h) : permute t l

-- | In the presence of overloaded strings, throwError freaks out
throwExc :: Member (Exc String) r => String -> Eff r a
throwExc = throwError
