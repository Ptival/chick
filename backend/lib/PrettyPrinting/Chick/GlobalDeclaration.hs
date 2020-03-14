{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module PrettyPrinting.Chick.GlobalDeclaration where

import Control.Monad.Reader
import Data.Default
import Data.Text.Prettyprint.Doc

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
        [ pretty $ unVariable v
        , pretty ':'
        , τDoc
        ]
    GlobalDef v τ t -> do
      τDoc <- prettyDocU @'Chick τ
      tDoc <- prettyDocU @'Chick t
      return $ fillSep
        [ pretty $ unVariable v
        , pretty ':'
        , τDoc
        , ":="
        , tDoc
        ]
    GlobalInd i -> prettyDocU @'Chick i

instance PrettyPrintable 'Chick (GlobalDeclaration α Variable) where
  prettyDoc d = runReader (prettyDocU @'Chick d) def
