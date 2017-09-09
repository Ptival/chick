module Diff.ListFold where

data ΔListFold τ δτ a = ΔListFold
  { onInsert  ::     τ -> [τ] -> a -> a
  , onKeep    ::          [τ] -> a -> a
  , onModify  ::    δτ -> [τ] -> a -> a
  , onPermute :: [Int] -> [τ] -> a -> a
  , onRemove  ::          [τ] -> a -> a
  , onReplace ::   [τ] -> [τ] -> a -> a
  , onSame    ::          [τ] -> a -> a
  }
