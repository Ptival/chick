-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# OPTIONS_GHC -fno-warn-orphans #-}

-- module PrettyPrinting.OCaml.Binder
--   (
--   )
-- where

-- import Language (Language (OCaml))
-- import PrettyPrinting.PrettyPrintable
-- import Term.Binder
-- import Text.PrettyPrint.Annotated.WL

-- instance PrettyPrintable 'OCaml ν => PrettyPrintable 'OCaml (Binder ν) where
--   prettyDoc (Binder Nothing) = text "_"
--   prettyDoc (Binder (Just v)) = prettyDoc @'OCaml v
