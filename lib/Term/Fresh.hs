module Term.Fresh where

import Data.List

import Term.Binder
import Term.Free
import Term.Term
import Term.Variable

freshNames :: [String]
freshNames = [ (c:s) | s <- ("":freshNames), c <- ['a'..'z'] ]

freshVariables :: [Variable]
freshVariables = map Variable freshNames

freshAvoidStream :: [Variable] -> TermX ξ -> [Variable]
freshAvoidStream avoid t = freshVariables \\ (freeVars t `union` avoid)

freshAvoid :: [Variable] -> TermX ξ -> Variable
freshAvoid avoid t = head $ freshAvoidStream avoid t

freshAvoidStream2 :: [Variable] -> TermX ξ -> TermX ψ -> [Variable]
freshAvoidStream2 avoid t1 t2 =
  freshVariables \\ (freeVars t1 `union` freeVars t2 `union` avoid)

freshAvoid2 :: [Variable] -> TermX ξ -> TermX ψ -> Variable
freshAvoid2 avoid t1 t2 = head $ freshAvoidStream2 avoid t1 t2

freshStream :: TermX ξ -> [Variable]
freshStream = freshAvoidStream []

fresh :: TermX ξ -> Variable
fresh = freshAvoid []

freshStream2 :: TermX ξ -> TermX ψ -> [Variable]
freshStream2 = freshAvoidStream2 []

fresh2 :: TermX ξ -> TermX ψ -> Variable
fresh2 = freshAvoid2 []
