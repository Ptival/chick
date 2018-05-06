module Parsing.OCaml.AttrId
  ( attr_id_P
  ) where

import           Text.Megaparsec
import           Text.Megaparsec.String

import qualified OCaml.Parsing.ASTTypes as ASTTypes
import           OCaml.Parsing.ParseTree
import           Parsing.OCaml.Common
import           Parsing.OCaml.SingleAttrId
import           Parsing.OCaml.Tokens

attr_id_P :: Parser (ASTTypes.Loc String)
attr_id_P = choice
  [ do
    a <- try $ single_attr_id_P <* dot_T
    b <- attr_id_P
    return $ mkLoc (a ++ "^" ++ ASTTypes.txt b) none -- FIXME symbol_rloc
  , single_attr_id_P >>= \ a -> return $ mkLoc a none -- FIXME symbol_rloc
  ]
