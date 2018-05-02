{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Parsing.OCaml.DatatypeDeclaration
  ( core_type_list_P
  , simple_core_type2_P
  , type_declaration_P
  ) where

import Text.Megaparsec
import Text.Megaparsec.String

import OCaml
import Parsing.OCaml.Common
import Parsing.OCaml.Tokens

type_declaration_P :: Parser Type_declaration
type_declaration_P = do
  try $ type_T
  -- TODO: ext_attributes
  -- TODO: nonrec_flag
  -- TODO: optional_type_parameters
  n <- l_ident_T
  (kind, priv, manifest) <- type_kind_P
  -- TODO: constraints
  -- TODO: post_item_attributes
  return $ mkType [] [] kind priv manifest (mkRHS n 5)

type_kind_P :: Parser (Type_kind, Private_flag, Maybe Core_type)
type_kind_P = choice
  [ do
    equal_T
    t <- core_type_P
    return (Ptype_abstract, Public, Just t)
  , do
    equal_T
    cs <- constructor_declarations_P
    return (Ptype_variant (reverse cs), Private, Nothing)
  , return (Ptype_abstract, Public, Nothing)
  ]

constructor_declarations_P :: Parser [Constructor_declaration]
constructor_declarations_P = choice
  [ bar_T *> return []
  , (: []) <$> (bar_T *> constructor_declaration_P)
  , constructor_declaration_P `sepBy1` bar_T
  ]

constructor_declaration_P :: Parser Constructor_declaration
constructor_declaration_P = do
  name <- constr_ident_P
  (args, res) <- generalized_constructor_arguments_P
  -- attributes_P
  return $ constructor args res (mkRHS name 1)

generalized_constructor_arguments_P :: Parser (Constructor_arguments, Maybe a)
generalized_constructor_arguments_P = choice
  [ flip (,) Nothing <$> (of_T *> constructor_arguments_P)
    -- TODO: colon
  , return (Pcstr_tuple [], Nothing)
  ]

constructor_arguments_P :: Parser Constructor_arguments
constructor_arguments_P = choice
  [ Pcstr_tuple . reverse <$> core_type_list_P
  -- TODO: label declarations
  ]

core_type_list_P :: Parser [Core_type]
core_type_list_P = simple_core_type_P `sepBy` star_T

simple_core_type_P :: Parser Core_type
simple_core_type_P = choice
  [ simple_core_type2_P
  -- , parens core_type_comma_list
  ]

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

constructor ::
  Constructor_arguments ->
  Maybe Core_type ->
  Loc String ->
  Constructor_declaration
constructor {- loc attrs info -} args res name =
  Constructor_declaration
  { pcd_name = name
  , pcd_args = args
  , pcd_res  = res
  --, pcd_loc :: Location.t
  --, pcd_attributes :: attributes
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

simple_core_type2_P :: Parser Core_type
simple_core_type2_P = choice
  [ mkTyp . Ptyp_var <$> (quote_T *> ident_P)
  , mkTyp . const Ptyp_any <$> underscore_T
  , do
    t <- type_longident_P
    return $ mkTyp $ Ptyp_constr (mkRHS t 1) []
  ]

type_longident_P :: Parser Longident
type_longident_P = choice
  [ Lident <$> l_ident_T
  -- TODO
  ]

core_type_P :: Parser Core_type
core_type_P = choice
  [ core_type_no_attr_P
  -- , do
  --   t <- core_type
  --   a <- attribute
  --   return $ attr t a
  ]

core_type_no_attr_P :: Parser Core_type
core_type_no_attr_P = choice
  [ core_type2_P
  -- , TODO
  ]

core_type2_P :: Parser Core_type
core_type2_P = choice
  [ simple_core_type_or_tuple_P
  -- TODO
  ]

simple_core_type_or_tuple_P :: Parser Core_type
simple_core_type_or_tuple_P = choice
  [ simple_core_type_P
  -- TODO
  ]
