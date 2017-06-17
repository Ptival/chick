{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrettyPrinting.PrettyPrintableUnannotated where

import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import Precedence
import Term.Variable

class PrettyPrintableUnannotated t where
  prettyDocU :: (MonadReader PrecedenceTable m) => t Variable -> m (Doc ())
  prettyStrU :: t Variable -> String
  prettyStrU t =
    display . renderPretty 1.0 80 . runReader (prettyDocU t) $ def
