{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module PrettyPrinting.PrettyPrintableUnannotated (
  PrettyPrintableUnannotated(..),
  ) where

import Control.Monad.Reader
import Data.Default
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

import Precedence
import Language

class PrettyPrintableUnannotated (l :: Language) t where
  prettyDocU :: (MonadReader PrecedenceTable m) => t -> m (Doc ())
  prettyStrU :: t -> String
  prettyStrU t =
    renderString . layoutPretty defaultLayoutOptions . runReader (prettyDocU @l t) $ def
