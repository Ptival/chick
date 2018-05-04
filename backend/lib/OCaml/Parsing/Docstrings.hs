{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module OCaml.Parsing.Docstrings
  ( Docstring(..)
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
        { pexp_desc       = Pexp_constant (Const_string (ds_body ds) Nothing)
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
