{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.Pair where

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
    PrettyPrintableUnannotated 'Chick b
  ) =>
  PrettyPrintableUnannotated 'Chick (a, b)
  where
  prettyDocU (a, b) = do
    aDoc <- prettyDocU @'Chick a
    bDoc <- prettyDocU @'Chick b
    return $ encloseSep lbracket rbracket comma [aDoc, bDoc]

instance
  ( PrettyPrintable 'Chick a,
    PrettyPrintable 'Chick b
  ) =>
  PrettyPrintable 'Chick (a, b)
  where
  prettyDoc (a, b) =
    let aDoc = prettyDoc @'Chick a
     in let bDoc = prettyDoc @'Chick b
         in encloseSep lbracket rbracket comma [aDoc, bDoc]
