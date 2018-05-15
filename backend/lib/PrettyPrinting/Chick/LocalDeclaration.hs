{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PrettyPrinting.Chick.LocalDeclaration where

import Control.Monad.Reader
import Data.Default
import Data.Text.Prettyprint.Doc

import Language (Language(Chick))
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Term
import Typing.LocalDeclaration

instance PrettyPrintable 'Chick (LocalDeclaration α Variable) where
  prettyDoc d = runReader (prettyDocU @'Chick d) def

instance PrettyPrintableUnannotated 'Chick (LocalDeclaration α Variable) where
  prettyDocU = \case
    LocalAssum b τ -> do
      τDoc <- prettyDocU @'Chick τ
      return $ fillSep
        [ prettyDoc @'Chick b
        , pretty ':'
        , τDoc
        ]
    LocalDef v τ t -> do
      τDoc <- prettyDocU @'Chick τ
      tDoc <- prettyDocU @'Chick t
      return $ fillSep
        [ pretty $ unVariable v
        , pretty ':'
        , τDoc
        , ":="
        , tDoc
        ]
