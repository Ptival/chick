{-# LANGUAGE DeriveAnyClass #-}

module Precedence
  ( Precedence(..)
  , PrecedenceTable(..)
  , Tolerance(..)
  , isTolerable
  , tableToOrdering
  ) where

import Data.Default
import Data.List
import GHC.Generics
import Test.SmallCheck.Series

data Precedence
  = PrecMin
  | PrecMax
  | PrecAtom

  | PrecAnnot
  | PrecApp
  | PrecArrow
  | PrecLam
  | PrecLet
  | PrecMatch

  deriving (Eq, Generic, Serial m, Show)

newtype PrecedenceTable
  = PrecedenceTable [[Precedence]] -- from low to high, grouped when equal

instance Default PrecedenceTable where

 def = PrecedenceTable
   -- low precedence
   [ [PrecMin]                     -- 200 in Coq

   , [PrecLam, PrecLet, PrecMatch] -- 200 in Coq
   , [PrecAnnot]
   , [PrecArrow]                   --  99 in Coq (as a Reserved Notation)
   , [PrecApp]                     --  10 in Coq

   , [PrecMax, PrecAtom]           --   0 in Coq
   ]
   -- high precedence

data Tolerance
  = TolerateHigher
  | TolerateEqual
  | TolerateHigherThan Precedence
  | TolerateAny
  deriving (Eq)

isTolerable ::
  (Precedence -> Precedence -> Ordering) ->
  Precedence -> (Precedence, Tolerance) -> Bool
isTolerable ord child (parent, tolerance) =
  case tolerance of
  TolerateEqual        -> (>=?) $ child `ord` parent
  TolerateHigher       -> (>?)  $ child `ord` parent
  TolerateHigherThan p -> (>=?) $ child `ord` p
  TolerateAny          -> True
  where
    (>=?) :: Ordering -> Bool
    (>=?) LT = False
    (>=?) _  = True

    (>?) :: Ordering -> Bool
    (>?) GT = True
    (>?) _  = False

tableToOrdering :: PrecedenceTable -> (Precedence -> Precedence -> Ordering)
tableToOrdering (PrecedenceTable table) p1 p2 =
  compare (findIndex (elem p1) table) (findIndex (elem p2) table)
