{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.Universe where

import Control.Monad.Reader (runReader)
import Data.Default (Default (def))
import Language (Language (Chick))
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc, prettyStr),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (..),
  )
import Prettyprinter (Pretty (pretty))
import Term.Universe (Universe)

instance PrettyPrintableUnannotated 'Chick Universe where
  prettyDocU u = return $ pretty (show u)

instance PrettyPrintable 'Chick Universe where
  prettyDoc t = runReader (prettyDocU @'Chick t) def
  prettyStr = prettyStrU @'Chick
