{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parsing.OCaml.StructureItem.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
import Test.Tasty

import Parsing.OCaml.Structure
import Parsing.OCaml.StructureItem
import Parsing.TestUtils

structure_item_tests :: [String]
structure_item_tests =
  [ "type a = _"
  , "type a = 'b"
  , "type a = b"
  , "type a = |"
  , "type a = A"
  , "type a = A of t"
  , "type a = A of t | B of t"
  , "type a = A | B"
  , "type a = A | B | C"
  , "type a = | A | B | C"
  , [s|
type stats_type =
  | Reporting
  | Driver
  |]
  , "type time_perf = {rtime: float; utime: float; stime: float; cutime: float; cstime: float}"
  , "type perf_stats = {mem: mem_perf option; time: time_perf option}"
  , "type stats_kind = Time of Mtime_clock.counter"
  , "type stats_kind = Time of Unix.process_times"
  , "type stats_kind = Time of Mtime_clock.counter * Unix.process_times"
  , "type stats_kind = Time of Mtime_clock.counter * Unix.process_times | Memory | TimeAndMemory"
  , [s|
type stats_type =
  | ClangLinters
  | ClangFrontend
  | ClangFrontendLinters
  | JavaFrontend
  | PythonFrontend
  | Backend
  | Reporting
  | Driver
  |]
  , [s|
type stats_type =
  | ClangLinters of SourceFile.t
  | ClangFrontend of SourceFile.t
  | ClangFrontendLinters of SourceFile.t
  | JavaFrontend of SourceFile.t
  | PythonFrontend of SourceFile.t
  | Backend of SourceFile.t
  | Reporting
  | Driver
  |]
  , "open A"
  , "open !A"
  -- , "(* A *) open !A"
  , "open !A (* A *)"
  -- , "(* A *) open !A (* A *)"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.DatatypeDeclaration" $ []
  ++ map (mkParsingTest "structure_item_P" (structure_item_P structure_P)) structure_item_tests

test :: IO ()
test = defaultMain unitTests
