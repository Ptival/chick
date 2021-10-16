module Diff.Guess.Mapping
  ( Mapping,
  )
where

import Diff.Guess.Node (Node)

type Mapping = [(Node, Node)]
