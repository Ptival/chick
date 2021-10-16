-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

-- module PrettyPrinting.Coq.Declaration where

-- import qualified Inductive.Inductive as I
-- import Language (Language (Coq))
-- import PrettyPrinting.Coq.Definition ()
-- import PrettyPrinting.Coq.Inductive ()
-- import PrettyPrinting.PrettyPrintable
-- import PrettyPrinting.PrettyPrintableUnannotated
-- import Term.Term
-- import Text.PrettyPrint.Annotated.WL
-- import Vernacular

-- instance PrettyPrintableUnannotated 'Coq (Vernacular α Variable) where
--   prettyDocU = \case
--     Definition defn -> do
--       defDoc <- prettyDocU @'Coq defn
--       return $
--         hcat
--           [ defDoc,
--             text "."
--           ]
--     Inductive ind -> do
--       indDoc <- prettyDocU @'Coq ind
--       return $
--         hcat
--           [ indDoc,
--             text "."
--           ]
--     Unsupported s -> do
--       return $ text s

-- instance PrettyPrintable 'Coq (Vernacular α Variable) where
--   prettyDoc v = runReader (prettyDocU v) def
