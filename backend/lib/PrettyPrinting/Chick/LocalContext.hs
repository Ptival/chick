{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.LocalContext
  ( LocalContext (..),
    TypeCheckedLocalContext,
    addHyp,
    addLocalAssum,
    boundNames,
    lookupType,
  )
where

import Control.Monad.Reader (runReader)
import Data.Default (Default (def))
import Language (Language (Chick))
import PrettyPrinting.Chick.LocalDeclaration ()
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import Prettyprinter (comma, encloseSep, lbracket, rbracket)
import Term.Variable (Variable)
import Typing.LocalContext
  ( LocalContext (..),
    TypeCheckedLocalContext,
    addHyp,
    addLocalAssum,
    boundNames,
    lookupType,
  )

instance PrettyPrintableUnannotated 'Chick (LocalContext α Variable) where
  prettyDocU (LocalContext ctxt) =
    encloseSep lbracket rbracket comma <$> mapM (prettyDocU @'Chick) (reverse ctxt)

instance PrettyPrintable 'Chick (LocalContext α Variable) where
  prettyDoc c = runReader (prettyDocU @'Chick c) def
