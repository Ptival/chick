{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.Triple where

--import Control.Monad.Reader
--import Data.Default

import Language (Language (Chick))
import PrettyPrinting.Chick.GlobalDeclaration ()
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import Prettyprinter (comma, encloseSep, lbracket, rbracket)

instance
  ( PrettyPrintableUnannotated 'Chick a,
    PrettyPrintableUnannotated 'Chick b,
    PrettyPrintableUnannotated 'Chick c
  ) =>
  PrettyPrintableUnannotated 'Chick (a, b, c)
  where
  prettyDocU (a, b, c) = do
    aDoc <- prettyDocU @'Chick a
    bDoc <- prettyDocU @'Chick b
    cDoc <- prettyDocU @'Chick c
    return $ encloseSep lbracket rbracket comma [aDoc, bDoc, cDoc]

instance
  ( PrettyPrintable 'Chick a,
    PrettyPrintable 'Chick b,
    PrettyPrintable 'Chick c
  ) =>
  PrettyPrintable 'Chick (a, b, c)
  where
  prettyDoc (a, b, c) =
    let aDoc = prettyDoc @'Chick a
     in let bDoc = prettyDoc @'Chick b
         in let cDoc = prettyDoc @'Chick c
             in encloseSep lbracket rbracket comma [aDoc, bDoc, cDoc]
