{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Diff.Guess.Node
  ( Node(..)
  , dice
  , isomorphic
  , label
  , nodeAndDescendants
  , nodeAndDescendantsPostOrder
  , nodeDescendants
  , product
  )where

import           Prelude hiding (product)
import           Text.Printf
import           Util (count)

import qualified Data.List as List
import           Language (Language(Chick))
import           PrettyPrinting.PrettyPrintable
import           PrettyPrinting.Term ()
import           Term.Term
import qualified Term.Raw as Raw

product :: [Node] -> [Node] -> [(Node, Node)]
product l1 l2 = [ (a, b) | a <- l1, b <- l2 ]

{- dice computes a ratio of subnodes that are mapped together -}
dice :: [(Node, Node)] -> Node -> Node -> Double
dice m t1 t2 = (2 * fromIntegral c) / (lengthOf s1 + lengthOf s2)
  where
    lengthOf = fromIntegral . List.length
    c = count (`elem` m) (product s1 s2)
    s1 = nodeDescendants t1
    s2 = nodeDescendants t2

isomorphic :: Node -> Node -> Bool
isomorphic n1 n2 =
  height n1 == height n2 -- maybe this helps performance?
  && node n1 == node n2

-- NOTE: Taking care of annotations ends up being more work than I could care for
-- because we must keep track of parent annotations for all children so that we
-- know how to annotated when reconstructing.  It's simpler to just work on raw
-- terms for now.
data Node = Node
  { children   :: [Node]
  , height     :: Int
  , identifier :: Int
  , node       :: Raw.Term Variable
  , parent     :: Maybe Node
  }

instance Eq Node where
  (==) n1 n2 = identifier n1 == identifier n2

instance Show Node where
  show n = printf "(%s : %s)" (show . identifier $ n) (preview @'Chick . node $ n)

nodeDescendants :: Node -> [Node]
nodeDescendants t = c ++ concatMap nodeDescendants c
  where c = children t

nodeAndDescendants :: Node -> [Node]
nodeAndDescendants n = n : nodeDescendants n

nodeAndDescendantsPostOrder :: Node -> [Node]
nodeAndDescendantsPostOrder n =
  concatMap nodeAndDescendantsPostOrder (children n) ++ [n]

label :: Node -> String
label n = case node n of
  Annot _ _ _        -> "Annot"
  App   _ _ _        -> "App"
  Hole  _            -> "Hole"
  Lam   _ _          -> "Lam"
  Let   _ _ _        -> "Let"
  Match _ _ _        -> "Match"
  Pi    _ _ _        -> "Pi"
  Type  _            -> "Type"
  Var   _ v          -> printf "Var(%s)" (show v)
  UnsupportedOCaml _ -> "UnsupportedOCaml"
