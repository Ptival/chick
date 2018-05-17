{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

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

testProgram :: String
testProgram = [s|
open Lexing
open Ast
open Env

type tconstantc_module = TCModule of tfdec list
[@@deriving show, eq]

and tfdec' = { t_name:string; t_params:param list; t_rty:ctype; t_rlbl:label; t_body:tblock }
[@@deriving show, eq]

and tfdec = tfdec' pos_ast [@@deriving show, eq]

and tstm' =
  | TVarDec of string * labeled_type * texpr
  | TAssign of string * texpr
  | TArrAssign of string * texpr * texpr
  | TIf of texpr * tblock * tblock
  | TFor of string * ctype * texpr * texpr * tblock
  | TReturn of texpr
[@@deriving show, eq]

and tstm = tstm' pos_ast [@@deriving show, eq]
|]

test :: Maybe [Vernacular () Variable]
test = map fromOCaml <$> parseMaybe implementation_P testProgram

prettyTest :: Maybe String
prettyTest = prettyStrU @'Chick <$> test
