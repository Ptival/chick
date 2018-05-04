module OCaml.Parsing.ParseTree
  ( Attribute
  , Attributes
  , Constant(..)
  , Constructor_arguments(..)
  , Constructor_declaration(..)
  , Core_type(..)
  , Core_type_desc(..)
  , Expression(..)
  , Expression_desc(..)
  , Extension
  , Label_declaration(..)
  , Loc(..)
  , Longident(..)
  , Mutable_flag(..)
  , Pattern
  , Payload(..)
  , Private_flag(..)
  , Rec_flag(..)
  , Structure
  , Structure_item(..)
  , Structure_item_desc(..)
  , Type_declaration(..)
  , Type_kind(..)
  , Variance(..)
  , constructor
  , field
  , none
  ) where

import qualified OCaml.Parsing.ASTTypes as ASTTypes
import OCaml.Parsing.Location

data Constant
  = Pconst_integer String (Maybe Char)
  | Pconst_char Char
  | Pconst_string String (Maybe String)
  | Pconst_float String (Maybe Char)

type Attribute = (Loc String, Payload)
type Extension = (Loc String, Payload)
type Attributes = [Attribute]

data Payload
  = PStr Structure
  | PSig Signature
  | PTyp Core_type
  | PPat Pattern (Maybe Expression)

data Core_type = Core_type
  { ptyp_desc       :: Core_type_desc
  -- , ptyp_loc        :: Location.t
  -- , ptyp_attributes :: Attributes
  }
  deriving (Show)

data Core_type_desc
  = Ptyp_any
  | Ptyp_var String
  -- | Ptyp_arrow of Asttypes.arg_label * core_type * core_type
  | Ptyp_tuple [Core_type]
  | Ptyp_constr (Loc Longident) [Core_type]
  -- | Ptyp_object of object_field list * Asttypes.closed_flag
  -- | Ptyp_class of Longident.t Asttypes.loc * core_type list
  -- | Ptyp_alias of core_type * string
  -- | Ptyp_variant of row_field list * Asttypes.closed_flag * Asttypes.label list option
  -- | Ptyp_poly of string Asttypes.loc list * core_type
  -- | Ptyp_package of package_type
  -- | Ptyp_extension of extension
  deriving (Show)

data Constructor_arguments
  = Pcstr_tuple [Core_type]
  -- | Pcstr_record [Label_declaration]
  deriving (Show)

data Private_flag
  = Private
  | Public
  deriving (Show)

data Loc a = Loc
  { txt :: a
  , loc :: Location
  }
  deriving (Show)

data Type_declaration = Type_declaration
  { ptype_name :: Loc String
  , ptype_params :: [(Core_type, Variance)]
  , ptype_cstrs :: [(Core_type, Core_type, Location)]
  , ptype_kind :: Type_kind
  , ptype_private :: Private_flag
  , ptype_manifest :: Maybe Core_type
  --, ptype_attributes :: attributes
  --, ptype_loc :: Location.t
  }
  deriving (Show)

data Type_kind
  = Ptype_abstract
  | Ptype_variant [Constructor_declaration]
  | Ptype_record [Label_declaration]
  | Ptype_open
  deriving (Show)

data Label_declaration = Label_declaration
  { pld_name :: Loc String
  , pld_mutable :: Mutable_flag
  , pld_type :: Core_type
  -- , pld_loc :: Location.t
  -- , pld_attributes :: attributes
  }
  deriving (Show)

data Constructor_declaration = Constructor_declaration
  { pcd_name :: Loc String
  , pcd_args :: Constructor_arguments
  , pcd_res :: Maybe Core_type
  -- , pcd_loc :: Location
  -- , pcd_attributes :: attributes
  }
  deriving (Show)

data Mutable_flag
  = Immutable
  | Mutable
  deriving (Show)

data Rec_flag
  = Nonrecursive
  | Recursive
  deriving (Show)

data Variance
  = Covariant
  | Contravariant
  | Invariant
  deriving (Show)

data Longident
  = Lident String
  | Ldot Longident String
  | Lapply Longident Longident
  deriving (Show)

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

field ::
  Mutable_flag ->
  Loc String ->
  Core_type ->
  Label_declaration
field {- loc attrs info -} mut name typ =
  Label_declaration
  { pld_name    = name
  , pld_mutable = mut
  , pld_type    = typ
  --, pld_loc :: Location.t
  --, pld_attributes :: attributes
  }

type Structure = [Structure_item]

data Structure_item = Structure_item
  { pstr_desc :: Structure_item_desc
  , pstr_loc  :: Location
  }

data Structure_item_desc
  = Pstr_eval Expression Attributes
  -- | Pstr_value  Asttypes.rec_flag * value_binding list
  -- | Pstr_primitive  value_description
  | Pstr_type Rec_flag [Type_declaration]
  -- | Pstr_typext  type_extension
  -- | Pstr_exception  extension_constructor
  -- | Pstr_module  module_binding
  -- | Pstr_recmodule  module_binding list
  -- | Pstr_modtype  module_type_declaration
  -- | Pstr_open  open_description
  -- | Pstr_class  class_declaration list
  -- | Pstr_class_type  class_type_declaration list
  -- | Pstr_include  include_declaration
  | Pstr_attribute Attribute
  | Pstr_extension Extension Attributes

data Expression = Expression
  { pexp_desc       :: Expression_desc
  , pexp_loc        :: Location
  , pexp_attributes :: Attributes
  }

data Expression_desc
  = Pexp_ident Longident Location
  | Pexp_constant ASTTypes.Constant
  -- | Pexp_let Asttypes.rec_flag * value_binding list * expression
  -- | Pexp_function case list
  -- | Pexp_fun Asttypes.arg_label * expression option * pattern * expression
  -- | Pexp_apply expression * (Asttypes.arg_label * expression) list
  -- | Pexp_match expression * case list
  -- | Pexp_try expression * case list
  -- | Pexp_tuple expression list
  -- | Pexp_construct Longident.t Asttypes.loc * expression option
  -- | Pexp_variant Asttypes.label * expression option
  -- | Pexp_record (Longident.t Asttypes.loc * expression) list * expression option
  -- | Pexp_field expression * Longident.t Asttypes.loc
  -- | Pexp_setfield expression * Longident.t Asttypes.loc * expression
  -- | Pexp_array expression list
  -- | Pexp_ifthenelse expression * expression * expression option
  | Pexp_sequence Expression Expression
  -- | Pexp_while expression * expression
  -- | Pexp_for pattern * expression * expression * Asttypes.direction_flag * expression
  -- | Pexp_constraint expression * core_type
  -- | Pexp_coerce expression * core_type option * core_type
  -- | Pexp_send expression * Asttypes.label Asttypes.loc
  -- | Pexp_new Longident.t Asttypes.loc
  -- | Pexp_setinstvar Asttypes.label Asttypes.loc * expression
  -- | Pexp_override (Asttypes.label Asttypes.loc * expression) list
  -- | Pexp_letmodule string Asttypes.loc * module_expr * expression
  -- | Pexp_letexception extension_constructor * expression
  -- | Pexp_assert expression
  -- | Pexp_lazy expression
  -- | Pexp_poly expression * core_type option
  -- | Pexp_object class_structure
  -- | Pexp_newtype string Asttypes.loc * expression
  -- | Pexp_pack module_expr
  -- | Pexp_open Asttypes.override_flag * Longident.t Asttypes.loc * expression
  -- | Pexp_extension extension
  -- | Pexp_unreachable

type Signature = [Signature_item]

data Signature_item = Signature_item
  { psig_desc :: Signature_item_desc
  , psig_loc  :: Location
  }

data Signature_item_desc
  = Psig_value Value_description
  -- | Psig_type Asttypes.rec_flag * type_declaration list
  -- | Psig_typext type_extension
  -- | Psig_exception extension_constructor
  -- | Psig_module module_declaration
  -- | Psig_recmodule module_declaration list
  -- | Psig_modtype module_type_declaration
  -- | Psig_open open_description
  -- | Psig_include include_description
  -- | Psig_class class_description list
  -- | Psig_class_type class_type_declaration list
  -- | Psig_attribute attribute
  -- | Psig_extension extension * attributes

data Value_description = Value_description
  { pval_name       :: Loc String
  , pval_type       :: Core_type
  , pval_prim       :: [String]
  , pval_attributes :: Attributes
  , pval_loc        :: Location
  }

data Pattern = Pattern
  { ppat_desc       :: Pattern_desc
  , ppat_loc        :: Location
  , ppat_attributes :: Attributes
  }

data Pattern_desc
  = Ppat_any
  | Ppat_var (Loc String)
  -- | Ppat_alias pattern * string Asttypes.loc
  -- | Ppat_constant constant
  -- | Ppat_interval constant * constant
  -- | Ppat_tuple pattern list
  -- | Ppat_construct Longident.t Asttypes.loc * pattern option
  -- | Ppat_variant Asttypes.label * pattern option
  -- | Ppat_record (Longident.t Asttypes.loc * pattern) list * Asttypes.closed_flag
  -- | Ppat_array pattern list
  -- | Ppat_or pattern * pattern
  -- | Ppat_constraint pattern * core_type
  -- | Ppat_type Longident.t Asttypes.loc
  -- | Ppat_lazy pattern
  -- | Ppat_unpack string Asttypes.loc
  -- | Ppat_exception pattern
  -- | Ppat_extension extension
  -- | Ppat_open Longident.t Asttypes.loc * pattern
