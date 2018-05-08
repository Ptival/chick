{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.TypeDeclarations
  ( type_declarations_P
  ) where

import           Text.Megaparsec.String

import qualified OCaml.Parsing.ASTTypes as ASTTypes
import           OCaml.Parsing.ParseTree
import           Parsing.OCaml.AndTypeDeclaration
import           Parsing.OCaml.TypeDeclaration
import           Parsing.Utils

type_declarations_P :: Parser Structure -> Parser (ASTTypes.Rec_flag, [Type_declaration])
type_declarations_P structure_P = leftRecursive
  [ do
    (nonrec_flag, ty) <- type_declaration_P
    return $ (nonrec_flag, [ty])
  ]
  [ do
    ty <- and_type_declaration_P structure_P
    return $ \ (nonrec_flag, tys) -> (nonrec_flag, ty : tys)
  ]
