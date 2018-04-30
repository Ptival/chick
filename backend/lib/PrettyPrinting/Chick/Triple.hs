{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PrettyPrinting.Chick.Triple where

import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import Language (Language(Chick))
import PrettyPrinting.Chick.GlobalDeclaration ()
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated

instance
  ( PrettyPrintableUnannotated 'Chick a
  , PrettyPrintableUnannotated 'Chick b
  , PrettyPrintableUnannotated 'Chick c
  ) => PrettyPrintableUnannotated 'Chick (a, b, c)
  where
  prettyDocU (a, b, c) = do
    aDoc <- prettyDocU @'Chick a
    bDoc <- prettyDocU @'Chick b
    cDoc <- prettyDocU @'Chick c
    return $ encloseSep lbracket rbracket comma [aDoc, bDoc, cDoc]

instance
  ( PrettyPrintableUnannotated 'Chick a
  , PrettyPrintableUnannotated 'Chick b
  , PrettyPrintableUnannotated 'Chick c
  ) => PrettyPrintable 'Chick (a, b, c)
  where
  prettyDoc γ = runReader (prettyDocU @'Chick γ) def
