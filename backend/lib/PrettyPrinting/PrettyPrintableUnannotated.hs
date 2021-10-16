{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (..),
  )
where

import Control.Monad.Reader (MonadReader, runReader)
import Data.Default (Default (def))
import Language (Language)
import Precedence (PrecedenceTable)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)

class PrettyPrintableUnannotated (l :: Language) t where
  prettyDocU :: (MonadReader PrecedenceTable m) => t -> m (Doc ())
  prettyStrU :: t -> String
  prettyStrU t =
    renderString . layoutPretty defaultLayoutOptions . runReader (prettyDocU @l t) $ def
