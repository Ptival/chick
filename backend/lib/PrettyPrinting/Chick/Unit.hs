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

module PrettyPrinting.Chick.Unit where

import Text.PrettyPrint.Annotated.WL

import Language (Language(Chick))
import PrettyPrinting.Chick.GlobalDeclaration ()
import PrettyPrinting.Chick.Inductive ()
import PrettyPrinting.Chick.Term ()
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated

instance PrettyPrintableUnannotated 'Chick () where
  prettyDocU = return . prettyDoc @'Chick

instance PrettyPrintable 'Chick () where
  prettyDoc () = text "()"