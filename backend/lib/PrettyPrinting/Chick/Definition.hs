{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.Definition
  (
  )
where

import Control.Monad.Reader (runReader)
import Data.Default (Default (def))
import Definition
  ( Definition
      ( definitionKind,
        definitionName,
        definitionTerm,
        definitionType
      ),
  )
import Language (Language (Chick))
import PrettyPrinting.Chick.DefinitionObjectKind ()
import PrettyPrinting.Chick.Variable ()
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import PrettyPrinting.Term ()
import Prettyprinter (hcat, softline, space)
import Term.Variable (Variable)

instance PrettyPrintableUnannotated 'Chick (Definition α Variable) where
  prettyDocU d = do
    τDoc <- prettyDocU @'Chick $ definitionType d
    tDoc <- prettyDocU @'Chick $ definitionTerm d
    return $
      hcat
        [ prettyDoc @'Chick (definitionKind d),
          space,
          prettyDoc @'Chick (definitionName d),
          softline,
          ":",
          space,
          τDoc,
          softline,
          ":=",
          softline,
          tDoc
        ]

instance PrettyPrintable 'Chick (Definition α Variable) where
  prettyDoc v = runReader (prettyDocU @'Chick v) def
