module Parsing.OCaml.Implementation.Test
  ( test
  , unitTests
  ) where

import Test.Tasty

import Parsing.OCaml.Implementation
import Parsing.TestUtils

prefix :: FilePath
prefix = "test/Parsing/OCaml/Implementation/"

files :: [FilePath]
files = map (prefix ++)
  [ "infer_00.ml"
  , "infer_01.ml"
  , "infer_02.ml"
  , "infer_03.ml"
  , "infer_04.ml"
  , "infer_05.ml"
  ]

unitTests :: TestTree
unitTests = testGroup "Parsing.OCaml.Implementation" $ []
  ++ map (mkParsingTestFromFile "implementation_P" implementation_P) files

test :: IO ()
test = defaultMain unitTests
