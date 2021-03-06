{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module PrettyPrinting.Chick.List where

import Prettyprinter

import Language (Language(Chick))
import PrettyPrinting.Chick.GlobalDeclaration ()
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated

instance
  ( PrettyPrintableUnannotated 'Chick a
  ) => PrettyPrintableUnannotated 'Chick [a]
  where
    prettyDocU l = do
      lDoc <- mapM (prettyDocU @'Chick) l
      return $ encloseSep lbracket rbracket comma lDoc

instance
  ( PrettyPrintable 'Chick a
  ) => PrettyPrintable 'Chick [a]
  where
    prettyDoc l =
      let lDoc = map (prettyDoc @'Chick) l in
      encloseSep lbracket rbracket comma lDoc
