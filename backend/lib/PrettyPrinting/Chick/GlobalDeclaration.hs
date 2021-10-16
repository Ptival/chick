{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.GlobalDeclaration where

import Control.Monad.Reader (runReader)
import Data.Default (Default (def))
import Language (Language (Chick))
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import Prettyprinter (Pretty (pretty), fillSep)
import Term.Variable (Variable (..))
import Typing.GlobalDeclaration
  ( GlobalDeclaration (GlobalAssum, GlobalDef, GlobalInd),
  )

instance PrettyPrintableUnannotated 'Chick (GlobalDeclaration α Variable) where
  prettyDocU = \case
    GlobalAssum v τ -> do
      τDoc <- prettyDocU @'Chick τ
      return $
        fillSep
          [ pretty $ unVariable v,
            pretty ':',
            τDoc
          ]
    GlobalDef v τ t -> do
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
    GlobalInd i -> prettyDocU @'Chick i

instance PrettyPrintable 'Chick (GlobalDeclaration α Variable) where
  prettyDoc d = runReader (prettyDocU @'Chick d) def
