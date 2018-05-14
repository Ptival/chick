{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module PrettyPrinting.Chick.Declaration
  (
  ) where

-- import           Control.Monad.Reader
-- import           Data.Default
-- import           Text.PrettyPrint.Annotated.WL

-- import           Language (Language(Chick))
-- import           PrettyPrinting.Chick.Definition ()
-- import           PrettyPrinting.Chick.Inductive ()
-- import           PrettyPrinting.PrettyPrintable
-- import           PrettyPrinting.PrettyPrintableUnannotated
-- import           Term.Term
-- import           Vernacular

-- instance PrettyPrintableUnannotated 'Chick (Vernacular α Variable) where
--   prettyDocU = \case
--     Definition defn -> do
--       defDoc <- prettyDocU @'Chick defn
--       return $ hcat
--         [ defDoc
--         , text "."
--         ]

--     Inductive ind -> do
--       indDoc <- prettyDocU @'Chick ind
--       return $ hcat
--         [ indDoc
--         , text "."
--         ]

--     Unsupported s -> do
--       return $ text s

-- instance PrettyPrintable 'Chick (Vernacular α Variable) where
--   prettyDoc v = runReader (prettyDocU @'Chick v) def
