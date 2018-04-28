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

import           Inductive.Inductive
import           Language (Language(Chick))
import           PrettyPrinting.Chick.Constructor ()
import           PrettyPrinting.Chick.Term
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.PrettyPrintableUnannotated
import           Term.Term
import           Text.PrettyPrint.Annotated.WL

instance PrettyPrintableUnannotated 'Chick (Inductive α Variable) where
  prettyDocU (Inductive n ips iis u cs) = do
    psDoc <- mapM (boundTermDocVariable @'Chick) ips
    csDoc <- mapM (prettyDocU @'Chick) cs
    isDoc <- (prettyDocU @'Chick) (inductiveFamilyType' iis u)
    -- isDoc <- mapM boundTermDocBinder is
    -- isDoc <- mapM boundTermDocVariable is
    return $ vsep $
      [ fillSep $
        [ text "Inductive"
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
        [ text ":"
        -- , arrows (isDoc ++ [text "Type"])
        , isDoc
        , text ":="
        ]
      ]
      ++ (map (\ x -> fillSep [ text "|", x]) csDoc)

instance PrettyPrintable 'Chick (Inductive α Variable) where
  prettyDoc i = runReader (prettyDocU @'Chick i) def
