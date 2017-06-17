module Parsing.Precedence where

import Prelude
import Data.Array (elem, findIndex)
import Data.Generic (class Generic, gEq)
import PrettyPrint.PrettyPrint (Doc, nest, parens)

data Precedence
  = PrecMin
  | PrecMax
  | PrecAtom

  | PrecAnnot
  | PrecApp
  | PrecArrow
  | PrecLam
  | PrecLet

derive instance genericPrecedence :: Generic Precedence

instance eqPrecedence :: Eq Precedence where
  eq = gEq

newtype PrecedenceTable
  = PrecedenceTable (Array (Array Precedence)) -- from low to high, grouped when equal

defaultPrecedenceTable :: PrecedenceTable
defaultPrecedenceTable =
  PrecedenceTable
  -- low precedence
  [ [PrecMin]              -- 200 in Coq

  , [PrecLam, PrecLet]     -- 200 in Coq
  , [PrecAnnot]
  , [PrecArrow]            --  99 in Coq (as a Reserved Notation)
  , [PrecApp]              --  10 in Coq

  , [PrecMax, PrecAtom]    --   0 in Coq
  ]
  -- high precedence

data Tolerance
  = TolerateHigher
  | TolerateEqual
  | TolerateHigherThan Precedence
  | TolerateAny

derive instance genericTolerance :: Generic Tolerance

isTolerable ::
  (Precedence -> Precedence -> Ordering) ->
  Precedence -> { precedence :: Precedence, tolerance :: Tolerance } -> Boolean
isTolerable ord child { precedence : parent, tolerance } =
  case tolerance of
  TolerateEqual        -> isGE $ child `ord` parent
  TolerateHigher       -> isGT $ child `ord` parent
  TolerateHigherThan p -> isGE $ child `ord` p
  TolerateAny          -> true
  where
    isGE :: Ordering -> Boolean
    isGE LT = false
    isGE _  = true

    isGT :: Ordering -> Boolean
    isGT GT = true
    isGT _  = false

tableToOrdering :: PrecedenceTable -> (Precedence -> Precedence -> Ordering)
tableToOrdering (PrecedenceTable table) p1 p2 =
  compare (findIndex (elem p1) table) (findIndex (elem p2) table)

par ::
  ∀ a.
  PrecedenceTable ->
  { precedence :: Precedence, tolerance :: Tolerance } ->
  { doc :: Doc a, precedence :: Precedence } ->
  Doc a
par precs { precedence : pOut, tolerance } { doc, precedence : pIn } =
  if isTolerable (tableToOrdering precs) pIn { precedence : pOut, tolerance }
  then doc
  else parens <<< nest 2 $ doc
