{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.LocalDeclaration where

import Control.Monad.Reader (runReader)
import Data.Default (Default (def))
import Language (Language (Chick))
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import Prettyprinter (Pretty (pretty), fillSep)
import Term.Variable (Variable (..))
import Typing.LocalDeclaration
  ( LocalDeclaration (LocalAssum, LocalDef),
  )

instance PrettyPrintable 'Chick (LocalDeclaration α Variable) where
  prettyDoc d = runReader (prettyDocU @'Chick d) def

instance PrettyPrintableUnannotated 'Chick (LocalDeclaration α Variable) where
  prettyDocU = \case
    LocalAssum b τ -> do
      τDoc <- prettyDocU @'Chick τ
      return $
        fillSep
          [ prettyDoc @'Chick b,
            pretty ':',
            τDoc
          ]
    LocalDef v τ t -> do
      τDoc <- prettyDocU @'Chick τ
      tDoc <- prettyDocU @'Chick t
      return $
        fillSep
          [ pretty $ unVariable v,
            pretty ':',
            τDoc,
            ":=",
            tDoc
          ]
