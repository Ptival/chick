module Parsing.OCaml.Common
  ( constr_ident_P
  , ident_P
  , mkLoc
  , mkRHS
  , mkTyp
  , mkType
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Tokens
import Parsing.Utils

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

mkTyp :: Core_type_desc -> Core_type
mkTyp p = Core_type { ptyp_desc = p }

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

mkLoc :: a -> Location -> Loc a
mkLoc t l = Loc
  { txt = t
  , loc = l
  }

rhsLoc :: t -> ()
rhsLoc _ = ()

mkRHS :: a -> t -> Loc a
mkRHS rhs pos = mkLoc rhs (rhsLoc pos)
