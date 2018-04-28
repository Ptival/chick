{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module PrettyPrinting.Chick.LocalContext
  ( LocalContext(..)
  , TypeCheckedLocalContext
  , addHyp
  , addLocalAssum
  , boundNames
  , lookupType
  ) where

import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import Language (Language(Chick))
import Typing.LocalContext
import PrettyPrinting.Chick.LocalDeclaration ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Term

instance PrettyPrintableUnannotated 'Chick (LocalContext α Variable) where
  prettyDocU (LocalContext ctxt) =
    encloseSep lbracket rbracket comma <$> mapM (prettyDocU @'Chick) (reverse ctxt)

instance PrettyPrintable 'Chick (LocalContext α Variable) where
  prettyDoc c = runReader (prettyDocU @'Chick c) def
