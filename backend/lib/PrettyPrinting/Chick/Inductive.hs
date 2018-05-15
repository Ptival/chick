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

module PrettyPrinting.Chick.Inductive
  (
  ) where

import           Control.Monad.Reader
import           Data.Default
import           Data.Text.Prettyprint.Doc

import           Inductive.Inductive
import           Language (Language(Chick))
import           PrettyPrinting.Chick.Constructor ()
import           PrettyPrinting.Chick.Term
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Term

instance PrettyPrintableUnannotated 'Chick (Inductive α Variable) where
  prettyDocU (Inductive n ips iis u cs) = do
    psDoc <- mapM (boundTermDocVariable @'Chick) ips
    csDoc <- mapM (prettyDocU @'Chick) cs
    isDoc <- (prettyDocU @'Chick) (inductiveFamilyType' iis u)
    -- isDoc <- mapM boundTermDocBinder is
    -- isDoc <- mapM boundTermDocVariable is
    return $ vsep $
      [ fillSep $
        [ "Inductive"
        , prettyDoc @'Chick n
        ]
        ++
        (
          -- mempty creates an unwanted space, so have to use []
          if length ips == 0
          then []
          else [encloseSep mempty mempty mempty psDoc]
        )
        ++
        [ ":"
        -- , arrows (isDoc ++ ["Type"])
        , isDoc
        , ":="
        ]
      ]
      ++ (map (\ x -> fillSep [ "|", x]) csDoc)

instance PrettyPrintable 'Chick (Inductive α Variable) where
  prettyDoc i = runReader (prettyDocU @'Chick i) def
