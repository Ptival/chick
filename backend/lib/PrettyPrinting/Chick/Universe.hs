{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.Chick.Universe where

import Control.Monad.Reader
import Data.Default
import Data.Text.Prettyprint.Doc

import Language (Language(Chick))
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Universe

instance PrettyPrintableUnannotated 'Chick Universe where
  prettyDocU u = return $ pretty (show u)

instance PrettyPrintable 'Chick Universe where
  prettyDoc t = runReader (prettyDocU @'Chick t) def
  prettyStr = prettyStrU @'Chick
