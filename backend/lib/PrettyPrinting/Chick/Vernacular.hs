{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.Chick.Definition
  (
  ) where

import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import Definition
import Language (Language(Chick))
import PrettyPrinting.Chick.DefinitionObjectKind ()
import PrettyPrinting.Chick.Variable ()
import PrettyPrinting.Term ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Term

instance PrettyPrintableUnannotated 'Chick (Definition α Variable) where
  prettyDocU d = do
    τDoc <- prettyDocU @'Chick $ definitionType d
    tDoc <- prettyDocU @'Chick $ definitionTerm d
    return $ hcat
      [ prettyDoc @'Chick (definitionKind d)
      , space
      , prettyDoc @'Chick (definitionName d)
      , softline
      , text ":"
      , space
      , τDoc
      , softline
      , text ":="
      , softline
      , tDoc
      ]

instance PrettyPrintable 'Chick (Definition α Variable) where
  prettyDoc v = runReader (prettyDocU @'Chick v) def
