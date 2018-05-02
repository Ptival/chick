module OCaml
  ( Constructor_arguments(..)
  , Constructor_declaration(..)
  , Core_type(..)
  , Core_type_desc(..)
  , Label_declaration(..)
  , Loc(..)
  , Location
  , Longident(..)
  , Mutable_flag(..)
  , Private_flag(..)
  , Type_declaration(..)
  , Type_kind(..)
  , Variance(..)
  , constructor
  , field
  ) where

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

type Location = ()

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
