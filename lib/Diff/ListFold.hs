module Diff.ListFold where

data ΔListFold τ δτ a = ΔListFold
  { onInsert  ::     τ -> a -> a
  , onKeep    ::          a -> a
  , onModify  ::    δτ -> a -> a
  , onPermute :: [Int] -> a -> a
  , onRemove  ::          a -> a
  , onReplace ::   [τ] -> a -> a
  , onSame    ::          a -> a
  }
