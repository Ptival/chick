module Parsing.OCaml.Common
  ( constr_ident_P
  , ident_P
  , mkExp
  , mkLoc
  , mkRHS
  , mkstr_ext
  , mkstrexp
  , mkTyp
  , mkType
  ) where

import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.String

import OCaml.Parsing.ASTTypes
import OCaml.Parsing.Location
import OCaml.Parsing.ParseTree
import Parsing.OCaml.Tokens
import Parsing.Utils

default_loc :: Location
default_loc = none

constr_ident_P :: Parser String
constr_ident_P = lexeme $ choice
  [ u_ident_T
  , l_bracket_T *> r_bracket_T *> return "[]"
  -- TODO: other ones
  , false_T *> return "false"
  , true_T *> return "true"
  ]

ident_P :: Parser String
ident_P = lexeme $ choice [ u_ident_T, l_ident_T ]

mkStr :: Maybe Location -> Structure_item_desc -> Structure_item
mkStr l d =
  Structure_item
  { pstr_desc = d
  , pstr_loc  = fromMaybe default_loc l
  }

mkstr :: Structure_item_desc -> Structure_item
mkstr d = mkStr Nothing d -- FIXME: symbol_rloc

ghstr :: Structure_item_desc -> Structure_item
ghstr d = mkStr Nothing d -- FIXME: symbol_gloc

wrap_str_ext :: Structure_item -> Maybe (Loc String) -> Structure_item
wrap_str_ext body ext = case ext of
  Nothing -> body
  Just id -> ghstr $ Pstr_extension (id, PStr [body]) []

mkstr_ext :: Structure_item_desc -> Maybe (Loc String) -> Structure_item
mkstr_ext d ext = wrap_str_ext (mkstr d) ext

mkTyp :: Core_type_desc -> Core_type
mkTyp p = Core_type { ptyp_desc = p }

mkExp :: Maybe Location -> Maybe Attributes -> Expression_desc -> Expression
mkExp loc attrs desc =
  Expression
  { pexp_desc       = desc
  , pexp_loc        = fromMaybe default_loc loc
  , pexp_attributes = fromMaybe []          attrs
  }

mkType ::
  [(Core_type, Variance)] ->
  [(Core_type, Core_type, Location)] ->
  Type_kind ->
  Private_flag ->
  Maybe Core_type ->
  Loc String ->
  Type_declaration
mkType {- loc attrs docs text -} params cstrs kind priv manifest name =
  Type_declaration
  { ptype_name     = name
  , ptype_params   = params
  , ptype_cstrs    = cstrs
  , ptype_kind     = kind
  , ptype_private  = priv
  , ptype_manifest = manifest
  --, ptype_attributes :: attributes
  --, ptype_loc :: Location.t
  }

mkstrexp :: Expression -> Attributes -> Structure_item
mkstrexp e attrs = Structure_item
  { pstr_desc = Pstr_eval e attrs
  , pstr_loc = pexp_loc e
  }

mkLoc :: a -> Location -> Loc a
mkLoc t l = Loc
  { txt = t
  , loc = l
  }

rhsLoc :: t -> Location
rhsLoc _ = none -- FIXME

mkRHS :: a -> t -> Loc a
mkRHS rhs pos = mkLoc rhs (rhsLoc pos)

text_str pos = textStr (rhs_text pos)

attributeStr mloc a = mkStr mloc (Pstr_attribute a)

textStr :: [()] -> [Structure_item]
textStr txt = map (\ ds -> attributeStr Nothing (text_attr ds)) txt
