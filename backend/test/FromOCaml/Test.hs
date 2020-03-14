{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module FromOCaml.Test where

import Control.Monad
import Data.String.QQ

import Language.OCaml.Parser
import Language.OCaml.PrettyPrinter
import System.Directory
import System.FilePath.Posix

import FromOCaml
import Language
import PrettyPrinting.PrettyPrintableUnannotated
import Script
import Term.Term as Term

testDirectory :: String
testDirectory = "test/FromOCaml/ocaml"

getTestFiles :: IO [FilePath]
getTestFiles = do
  l <- listDirectory testDirectory
  return $ [ testDirectory ++ "/" ++ fileName
           | fileName <- filter ((==) ".ml" . takeExtension) l
           ]

_testProgram :: String
_testProgram = [s|
type 'a list =
  | Nil
  | Cons of ('a * 'a list)
|]

_test :: Either String (Script () Variable)
_test = Script . map fromOCaml <$> parseImplementation _testProgram

_prettyTest :: Either String String
_prettyTest = prettyStrU @'Chick <$> _test

main :: IO ()
main = do
  list <- getTestFiles
  forM_ list \ file -> do
    contents <- readFile file
    putStrLn file
    -- putStrLn contents
    case parseImplementation contents of
      Left  err -> putStrLn $ "ERR: " ++ show err
      Right ast -> do
        print (length ast)
        forM_ ast \ item -> print (structureItemPP item)
