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
(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Performance Statistics gathering and reporting *)

open! IStd
module F = Format
module L = Logging

type mem_perf =
  { minor_gb: float
  ; promoted_gb: float
  ; major_gb: float
  ; allocated_gb: float
  ; minor_collections: int
  ; major_collections: int
  ; compactions: int
  ; top_heap_gb: float
  ; stack_kb: float
  ; minor_heap_kb: float }

type time_perf = {rtime: float; utime: float; stime: float; cutime: float; cstime: float}

type perf_stats = {mem: mem_perf option; time: time_perf option}

type stats_kind = Time of Mtime_clock.counter * Unix.process_times | Memory | TimeAndMemory

type stats_type =
  | ClangLinters
  | ClangFrontend
  | ClangFrontendLinters
  | JavaFrontend
  | PythonFrontend
  | Backend
  | Reporting
  | Driver

let dirname_of_stats_type = function
  | ClangLinters ->
      Config.frontend_stats_dir_name
  | ClangFrontend ->
      Config.frontend_stats_dir_name
  | ClangFrontendLinters ->
      Config.frontend_stats_dir_name
  | JavaFrontend ->
      Config.frontend_stats_dir_name
  | PythonFrontend ->
      Config.frontend_stats_dir_name
  | Backend ->
      Config.backend_stats_dir_name
  | Reporting ->
      Config.reporting_stats_dir_name
  | Driver ->
      Config.driver_stats_dir_name

|]

_test :: Maybe [Structure_item]
_test = map (toOCaml . fromOCaml) <$> parseMaybe implementation_P _testProgram

_prettyTest :: Maybe String
_prettyTest = (show . structure_PP) <$> _test
