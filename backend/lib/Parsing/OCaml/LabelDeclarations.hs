{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Parsing.OCaml.LabelDeclarations
  ( label_declarations_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.LabelDeclaration
import Parsing.OCaml.LabelDeclarationSemi
import Parsing.OCaml.Tokens

label_declarations_P :: Parser [Label_declaration]
label_declarations_P = choice
  [ do
    h <- label_declaration_semi_P
    t <- label_declarations_P
    return $ h : t
  , (: []) <$> label_declaration_semi_P
  , (: []) <$> label_declaration_P
  ]
