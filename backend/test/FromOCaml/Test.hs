{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module FromOCaml.Test where

import Control.Monad
import Data.String.QQ

import FromOCaml
import Language
import Language.OCaml.Parser
import Language.OCaml.PrettyPrinter
import PrettyPrinting.PrettyPrintableUnannotated
import Script
import System.Directory
import System.FilePath.Posix
import Term.Term as Term

testDirectory :: [Char]
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
_test = Script . map fromOCaml <$> parseImplementationG _testProgram

_prettyTest :: Either String String
_prettyTest = prettyStrU @'Chick <$> _test

main :: IO ()
main = do
  list <- getTestFiles
  forM_ list $ \ file -> do
    contents <- readFile file
    putStrLn file
    -- putStrLn contents
    case parseImplementationG contents of
      Left  err -> putStrLn $ "ERR: " ++ show err
      Right ast -> do
        putStrLn $ show $ length ast
        forM_ ast $ \ item -> do
        putStrLn $ show (structureItemPP item)
