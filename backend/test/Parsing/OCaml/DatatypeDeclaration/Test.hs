{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parsing.OCaml.DatatypeDeclaration.Test
  ( test
  , unitTests
  ) where

import Data.String.QQ
-- import Text.Megaparsec
-- import Text.Megaparsec.String
import Test.Tasty

-- import OCaml
import Parsing.OCaml.DatatypeDeclaration
import Parsing.TestUtils

-- core_type_list_tests :: [String]
-- core_type_list_tests =
--   [ "a * b"
--   , ""
--   , "a"
--   , "*"
--   ]

-- simple_core_type2_tests :: [String]
-- simple_core_type2_tests =
--   [ "'x"
--   , "_"
--   , "a"
--   , "a b"
--   , "a b c"
--   ]

-- constructor_declaration_tests :: [String]
-- constructor_declaration_tests =
--   [ "A"
--   , "A of t"
--   ]

-- constructor_declarations_tests :: [String]
-- constructor_declarations_tests =
--   [ "A | B"
--   , "A of t | B of t"
--   ]

-- type_kind_tests :: [String]
-- type_kind_tests =
--   [ "= A | B"
--   , "= A of t | B of t"
--   ]

-- label_declarations_tests :: [String]
-- label_declarations_tests =
--   [ "rtime: float; utime: float; stime: float; cutime: float; cstime: float"
--   , "mem: mem_perf option; time: time_perf option"
--   ]

-- mod_ext_longident_tests :: [String]
-- mod_ext_longident_tests =
--   [ "Unix"
--   , "Unix.Unix"
--   , "Mtime_clock.Unix"
--   ]

-- type_longident_tests :: [String]
-- type_longident_tests =
--   [ "yolo"
--   , "Mtime_clock.counter"
--   , "Unix.process_times"
--   ]

type_declaration_tests :: [String]
type_declaration_tests =
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
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.DatatypeDeclaration" $ []
  -- ++ map (mkParsingTest "core_type_P" core_type_list_P) core_type_list_tests
  -- ++ map (mkParsingTest "simple_core_type2_P" simple_core_type2_P) simple_core_type2_tests
  -- ++ map (mkParsingTest "constructor_declaration_P" constructor_declaration_P) constructor_declaration_tests
  -- ++ map (mkParsingTest "constructor_declarations_P" constructor_declarations_P) constructor_declarations_tests
  -- ++ map (mkParsingTest "type_kind_P" type_kind_P) type_kind_tests
  -- ++ map (mkParsingTest "label_declarations_P" label_declarations_P) label_declarations_tests
  -- ++ map (mkParsingTest "mod_ext_longident_P" mod_ext_longident_P) mod_ext_longident_tests
  -- ++ map (mkParsingTest "type_longident_P" type_longident_P) type_longident_tests
  ++ map (mkParsingTest "type_declaration_P" type_declaration_P) type_declaration_tests

-- test1 = parseMaybe simple_core_type2_P "a b c"

test :: IO ()
test = defaultMain unitTests
