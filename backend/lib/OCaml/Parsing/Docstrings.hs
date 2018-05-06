{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module OCaml.Parsing.Docstrings
  ( Docs
  , Docstring(..)
  , add_docs_attrs
  , docs_attr
  , empty_docs
  , rhs_text
  , text_attr
  ) where

import OCaml.Parsing.ASTTypes
import OCaml.Parsing.Location
import OCaml.Parsing.ParseTree
import OCaml.StdLib.Parsing

data Ds_attached
   = Unattached   {- Not yet attached anything. -}
   | Info         {- Attached to a field or constructor. -}
   | Docs         {- Attached to an item or as floating text. -}

{- A docstring is "associated" with an item if there are no blank lines between
   them. This is used for generating docstring ambiguity warnings. -}
data Ds_associated
  = Zero             {- Not associated with an item -}
  | One              {- Associated with one item -}
  | Many

data Docstring = Docstring
  { ds_body       :: String
  , ds_loc        :: Location
  , ds_attached   :: Ds_attached
  , ds_associated :: Ds_associated
  }

text_loc :: Loc String
text_loc = Loc
  { txt = "ocaml.text"
  , loc = none
  }

text_attr :: Docstring -> Attribute
text_attr ds =
  let exp =
        Expression
        { pexp_desc       = Pexp_constant (Pconst_string (ds_body ds) Nothing)
        , pexp_loc        = ds_loc ds
        , pexp_attributes = []
        }
  in
  let item =
        Structure_item
        { pstr_desc = Pstr_eval exp []
        , pstr_loc  = pexp_loc exp
        }
  in
  (text_loc, PStr [item])

rhs_text :: a -> [b]
rhs_text pos = get_text (rhs_start_pos pos)

get_text :: a -> [b]
get_text _pos = [] -- FIXME

data Docs = Docs'
  { docs_pre  :: Maybe Docstring
  , docs_post :: Maybe Docstring
  }

add_docs_attrs :: Docs -> [(Loc String, Payload)] -> [(Loc String, Payload)]
add_docs_attrs docs attrs =
  let attrs1 = case docs_pre docs of
               Nothing -> attrs
               Just ds -> docs_attr ds : attrs
  in
  let attrs2 = case docs_post docs of
               Nothing -> attrs1
               Just ds -> attrs1 ++ [docs_attr ds]
  in
  attrs2

empty_docs :: Docs
empty_docs =
  Docs'
  { docs_pre = Nothing
  , docs_post = Nothing
  }

doc_loc :: Loc String
doc_loc =
  Loc
  { txt = "ocaml.doc"
  , loc = none
  }

docs_attr :: Docstring -> (Loc String, Payload)
docs_attr ds =
  let exp = Expression
        { pexp_desc = Pexp_constant $ Pconst_string (ds_body ds) Nothing
        , pexp_loc = ds_loc ds
        , pexp_attributes = []
        }
  in
  let item = Structure_item
        { pstr_desc = Pstr_eval exp []
        , pstr_loc = pexp_loc exp
        }
  in
  (doc_loc, PStr [item])
