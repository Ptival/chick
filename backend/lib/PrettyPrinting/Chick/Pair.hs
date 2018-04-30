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

module PrettyPrinting.Chick.Pair where

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
  ) => PrettyPrintableUnannotated 'Chick (a, b)
  where
  prettyDocU (a, b) = do
    aDoc <- prettyDocU @'Chick a
    bDoc <- prettyDocU @'Chick b
    return $ encloseSep lbracket rbracket comma [aDoc, bDoc]

instance
  ( PrettyPrintableUnannotated 'Chick a
  , PrettyPrintableUnannotated 'Chick b
  ) => PrettyPrintable 'Chick (a, b) where
  prettyDoc γ = runReader (prettyDocU @'Chick γ) def
