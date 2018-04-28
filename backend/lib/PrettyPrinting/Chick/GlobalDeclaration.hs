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

module PrettyPrinting.Chick.LocalDeclaration where

import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import Language (Language(Chick))
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Term
import Typing.GlobalDeclaration

instance PrettyPrintableUnannotated 'Chick (GlobalDeclaration α Variable) where
  prettyDocU = \case
    GlobalAssum v τ -> do
      τDoc <- prettyDocU @'Chick τ
      return $ fillSep
        [ text (unVariable v)
        , char ':'
        , τDoc
        ]
    GlobalDef v τ t -> do
      τDoc <- prettyDocU @'Chick τ
      tDoc <- prettyDocU @'Chick t
      return $ fillSep
        [ text (unVariable v)
        , char ':'
        , τDoc
        , text ":="
        , tDoc
        ]
    GlobalInd i -> prettyDocU @'Chick i

instance PrettyPrintable 'Chick (GlobalDeclaration α Variable) where
  prettyDoc d = runReader (prettyDocU @'Chick d) def
