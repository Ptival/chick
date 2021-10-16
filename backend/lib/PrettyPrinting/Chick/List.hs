{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.List where

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
  ( PrettyPrintableUnannotated 'Chick a
  ) =>
  PrettyPrintableUnannotated 'Chick [a]
  where
  prettyDocU l = do
    lDoc <- mapM (prettyDocU @'Chick) l
    return $ encloseSep lbracket rbracket comma lDoc

instance
  ( PrettyPrintable 'Chick a
  ) =>
  PrettyPrintable 'Chick [a]
  where
  prettyDoc l =
    let lDoc = map (prettyDoc @'Chick) l
     in encloseSep lbracket rbracket comma lDoc
