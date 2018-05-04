module OCaml.Parsing.ASTTypes
  ( Constant(..)
  ) where

data Constant
   = Const_int Int
   | Const_char Char
   | Const_string String (Maybe String)
   | Const_float String
   -- | Const_int32 Int32
   -- | Const_int64 Int64
   -- | Const_nativeint Nativeint
