{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FromOCaml
  ( FromOCaml(..)
  ) where

import Control.Arrow
import Data.String.QQ
import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.Parser
import Text.Megaparsec

import Definition
import DefinitionObjectKind
import Language
import PrettyPrinting.PrettyPrintableUnannotated
import PrettyPrinting.Chick ()
import Term.Term
import Utils
import Vernacular

class FromOCaml a b | a -> b where
  fromOCaml :: a -> b

instance FromOCaml Structure_item (Vernacular () Variable) where
  fromOCaml ocaml = (fromOCaml . pstr_desc $ ocaml) ocaml

instance FromOCaml Rec_flag DefinitionObjectKind where
  fromOCaml = \case
    Recursive    -> Fixpoint
    Nonrecursive -> DefinitionObjectKind.Definition

instance FromOCaml Structure_item_desc (Structure_item -> Vernacular () Variable) where
  fromOCaml ocaml = case ocaml of
    Pstr_value r [vb] ->
      case ppat_desc . pvb_pat $ vb of
      Ppat_var v -> const . Vernacular.Definition $ Definition.Definition
        { definitionKind = fromOCaml r
        , definitionName = mkVariable (txt v)
        , definitionType = Hole ()
        , definitionTerm = fromOCaml . pvb_expr $ vb
        }
      _ -> error "TODO"
    _ -> Vernacular.UnsupportedOCaml

instance FromOCaml Expression (TermX () Variable) where
  fromOCaml ocaml = (fromOCaml . pexp_desc $ ocaml) ocaml

instance FromOCaml Longident (Maybe Variable) where
  fromOCaml ocaml = case ocaml of
    Lident s -> Just $ mkVariable s
    Ldot _ _ -> Nothing
    Lapply _ _ -> Nothing

instance FromOCaml Expression_desc (Expression -> TermX () Variable) where
  fromOCaml ocaml = case ocaml of

    Pexp_apply f as ->
      const $ mkApps (fromOCaml f) (map ((const ()) *** fromOCaml) as)

    Pexp_function l ->
      let arg :: Variable = "__arg__" in
      const
      $ Lam () $ abstractVariable arg
      $ Match () (Var Nothing arg)
      $ map fromOCaml l

    Pexp_ident i -> case fromOCaml $ txt i of
      Nothing -> Term.Term.UnsupportedOCaml
      Just v -> const $ Var Nothing v

    Pexp_let Nonrecursive [vb] e ->
      case ppat_desc $ pvb_pat vb of
      Ppat_var v ->
        let e1 = fromOCaml . pvb_expr $ vb in
        let b  = mkBinder $ txt v in
        let e2 = fromOCaml e in
        const $ Let () e1 (abstractBinder b e2)
      _ -> Term.Term.UnsupportedOCaml

    _ -> error $ show ocaml

instance FromOCaml Case (Branch () Variable) where
  fromOCaml ocaml = case pc_guard ocaml of
    Nothing -> error "TODO"
    Just _ -> error "TODO"

_testProgram :: String
_testProgram = [s|
type 'a list =
  | Nil
  | Cons of ('a * 'a list)
|]

_test :: Maybe [Vernacular () Variable]
_test = map fromOCaml <$> parseMaybe implementation_P _testProgram

_prettyTest :: Maybe String
_prettyTest = prettyStrU @'Chick <$> _test
