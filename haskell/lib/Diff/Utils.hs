{-# language FlexibleContexts #-}

module Diff.Utils
 ( throwExc
 ) where

import Control.Monad.Freer
import Control.Monad.Freer.Exception

-- | In the presence of overloaded strings, throwError freaks out
throwExc :: Member (Exc String) r => String -> Eff r a
throwExc = throwError
