{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module PrettyPrinting.Chick.Constructor
  (
  ) where

import           Control.Monad.Reader
import           Data.Default

import           Inductive.Inductive
import           Language (Language(Chick))
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Term
import           Data.Text.Prettyprint.Doc

instance PrettyPrintableUnannotated 'Chick (Constructor α Variable) where
  prettyDocU (Constructor (Inductive n ips _ _ _) cName cParams cIndices) = do
    cDoc <- prettyDocU @'Chick (constructorRawType' False n ips cParams cIndices)
    return $ fillSep
      [ prettyDoc @'Chick cName
      , ":"
      , cDoc
      ]

instance PrettyPrintable 'Chick (Constructor α Variable) where
  prettyDoc c = runReader (prettyDocU @'Chick c) def
