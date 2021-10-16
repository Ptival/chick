{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PrettyPrinting.Chick.GlobalEnvironment where

import Control.Monad.Reader (runReader)
import Data.Default (Default (def))
import Language (Language (Chick))
import PrettyPrinting.Chick.GlobalDeclaration ()
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
  ( PrettyPrintable (prettyDoc),
  )
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyDocU),
  )
import Prettyprinter (comma, encloseSep, lbracket, rbracket)
import Term.Variable (Variable)
import Typing.GlobalEnvironment
  ( GlobalEnvironment (GlobalEnvironment),
  )

instance PrettyPrintableUnannotated 'Chick (GlobalEnvironment ξ Variable) where
  prettyDocU (GlobalEnvironment γ) =
    encloseSep lbracket rbracket comma <$> mapM (prettyDocU @'Chick) (reverse γ)

instance PrettyPrintable 'Chick (GlobalEnvironment ξ Variable) where
  prettyDoc γ = runReader (prettyDocU @'Chick γ) def
