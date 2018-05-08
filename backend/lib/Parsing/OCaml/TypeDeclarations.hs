{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Parsing.OCaml.TypeDeclarations
  ( type_declarations_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified OCaml.Parsing.ASTTypes as ASTTypes
import           OCaml.Parsing.ParseTree
import           Parsing.OCaml.AndTypeDeclaration
import           Parsing.OCaml.TypeDeclaration

type_declarations_P :: Parser Structure -> Parser (ASTTypes.Rec_flag, [Type_declaration])
type_declarations_P structure_P = do
    (nonrec_flag, ty) <- type_declaration_P
    r <- rest
    return . r $ (nonrec_flag, [ty])
  where
    rest = choice
      [ do
        ty <- and_type_declaration_P structure_P
        r <- rest
        return $ \ (nonrec_flag, tys) -> r $ (nonrec_flag, ty : tys)
      , return id
      ]

type_declarations_P :: Parser Structure -> Parser (ASTTypes.Rec_flag, [Type_declaration])
type_declarations_P structure_P = do
    (nonrec_flag, ty) <- type_declaration_P
    r <- rest
    return . r $ (nonrec_flag, [ty])
  where
    rest = choice
      [ do
        ty <- and_type_declaration_P structure_P
        r <- rest
        return $ \ (nonrec_flag, tys) -> r $ (nonrec_flag, ty : tys)
      , return id
      ]
