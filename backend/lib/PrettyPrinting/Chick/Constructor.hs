{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.Constructor
  (
  )
where

import Control.Monad.Reader (runReader)
import Data.Default (Default (def))
import Inductive.Inductive
  ( Constructor (Constructor),
    Inductive (Inductive),
    constructorRawType',
  )
import Language (Language (Chick))
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import Prettyprinter (fillSep)
import Term.Variable (Variable)

instance PrettyPrintableUnannotated 'Chick (Constructor α Variable) where
  prettyDocU (Constructor (Inductive n ips _ _ _) cName cParams cIndices) = do
    cDoc <- prettyDocU @'Chick (constructorRawType' False n ips cParams cIndices)
    return $
      fillSep
        [ prettyDoc @'Chick cName,
          ":",
          cDoc
        ]

instance PrettyPrintable 'Chick (Constructor α Variable) where
  prettyDoc c = runReader (prettyDocU @'Chick c) def
