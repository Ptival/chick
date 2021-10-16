-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

-- module PrettyPrinting.Coq.Binder where

-- import Language (Language (Coq))
-- import PrettyPrinting.PrettyPrintable
--   ( PrettyPrintable (prettyDoc),
--   )
-- import qualified Prettyprinter as Doc
-- import Term.Binder (Binder (Binder))

-- instance PrettyPrintable 'Coq ν => PrettyPrintable 'Coq (Binder ν) where
--   prettyDoc (Binder Nothing) = Doc.pretty "_"
--   prettyDoc (Binder (Just v)) = prettyDoc @'Coq v
