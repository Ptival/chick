{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module ToOCaml
  (

  ) where

import Data.String.QQ
import Language.OCaml.Parser.Common
import Language.OCaml.Definitions.Parsing.ASTTypes
import Language.OCaml.Definitions.Parsing.ParseTree
import Language.OCaml.PrettyPrinter
import Language.OCaml.Parser
import Text.Megaparsec

import Definition
import DefinitionObjectKind
import FromOCaml
import PrettyPrinting.Chick ()
import Term.Term
import Vernacular

class ToOCaml a b | a -> b where
  toOCaml :: a -> b

instance ToOCaml (Vernacular () Variable) Structure_item where
  toOCaml = \case
    Vernacular.Definition d ->
      let r = toOCaml $ definitionKind d in
      let pat = mkpat $ Ppat_var $ toOCaml $ definitionName d in
      let expr = toOCaml $ definitionTerm d in
      let vb = mkVb Nothing Nothing Nothing Nothing pat expr in
      mkStr Nothing $ Pstr_value r [vb]
    Inductive _i -> error "TODO"
    Vernacular.UnsupportedOCaml o -> o
    --mkStr Nothing

instance ToOCaml DefinitionObjectKind Rec_flag where
  toOCaml = \case
    Fixpoint                        -> Recursive
    DefinitionObjectKind.Definition -> Nonrecursive

instance ToOCaml (Structure_item -> Vernacular () Variable) Structure_item_desc where
  toOCaml = error "TODO"

instance ToOCaml (TermX () Variable) Expression where
  toOCaml chick = case chick of

    Let _ e1 be2 ->
      let (b, e2) = unscopeTerm be2 in
      let pat = toOCaml b in
      let expr = toOCaml e1 in
      let vb = mkVb Nothing Nothing Nothing Nothing pat expr in
      let e = toOCaml e2 in
      mkexp $ Pexp_let Nonrecursive [vb] e

    Var _ v -> mkexp $ Pexp_ident $ mkLoc (Lident (unVariable v)) none

    _ -> error $ show chick

instance ToOCaml Variable (Loc String) where
  toOCaml v = mkLoc (unVariable v) none

instance ToOCaml (Binder Variable) Pattern where
  toOCaml (Binder Nothing) = mkpat $ Ppat_any
  toOCaml (Binder (Just v)) = mkpat $ Ppat_var $ toOCaml v

--instance ToOCaml (Maybe Variable) Longident where

--instance ToOCaml (Expression -> TermX () Variable) Expression_desc where

_testProgram :: String
_testProgram = [s|
type 'a list =
  | Nil
  | Cons of ('a * 'a list)
|]

_test :: Maybe [Structure_item]
_test = map (toOCaml . fromOCaml) <$> parseMaybe implementation_P _testProgram

_prettyTest :: Maybe String
_prettyTest = (show . structure_PP) <$> _test
