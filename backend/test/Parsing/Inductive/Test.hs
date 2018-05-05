{-# LANGUAGE OverloadedStrings #-}

module Parsing.Inductive.Test
  ( test
  , unitTests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Inductive.Inductive
import           Parsing.Unsafe
import qualified StandardLibrary as SL
import qualified Term.Raw as Raw
import           Term.Term
import qualified Term.Universe as U

indAnd :: Inductive Raw.Raw Variable
indAnd =
  Inductive "and" [((), "A", Type U.Prop), ((), "B", Type U.Prop)] [] U.Prop
  [ conjAnd ]

conjAnd :: Constructor Raw.Raw Variable
conjAnd  = Constructor indAnd "conj" [((), "a", "A"), ((), "b", "B")] []

indBool :: Inductive Raw.Raw Variable
indBool =
  Inductive "bool" [] [] U.Set
  [ trueBool
  , falseBool
  ]

trueBool, falseBool :: Constructor Raw.Raw Variable
trueBool  = Constructor indBool "true"  [] []
falseBool = Constructor indBool "false" [] []

indEq :: Inductive Raw.Raw Variable
indEq =
  Inductive "eq" [((), "A", Type U.Type), ((), "x", "A")] [((), "other", "A")] U.Prop
  [ eqReflEq ]

eqReflEq :: Constructor Raw.Raw Variable
eqReflEq  = Constructor indEq "eq_refl" [] [((), "x")]

indNat :: Inductive Raw.Raw Variable
indNat =
  Inductive "nat" [] [] U.Set
  [ zeroNat
  , succNat
  ]

zeroNat, succNat :: Constructor Raw.Raw Variable
zeroNat = Constructor indNat "O" [] []
succNat = Constructor indNat "S" [((), "_", "nat")] []

indList :: Inductive Raw.Raw Variable
indList =
  Inductive "list" [((), "A", Type U.Type)] [] U.Type
  [ nilList
  , consList
  ]

nilList, consList :: Constructor Raw.Raw Variable
nilList  = Constructor indList "nil"  [] []
consList = Constructor indList "cons"
    [ ((), "x", "A")
    , ((), "xs", unsafeParseTerm "list A")
    ]
    []

indOr :: Inductive Raw.Raw Variable
indOr =
  Inductive "or" [((), "A", Type U.Prop), ((), "B", Type U.Prop)] [] U.Prop
  [ orIntroLOr, orIntroROr ]

orIntroLOr, orIntroROr :: Constructor Raw.Raw Variable
orIntroLOr  = Constructor indAnd "or_introl" [((), "a", "A")] []
orIntroROr  = Constructor indOr  "or_intror" [((), "b", "B")] []

indFin :: Inductive Raw.Raw Variable
indFin =
  Inductive "Fin" [] [((), "bound", "nat")] U.Set
  [ zeroFin
  , succFin
  ]

zeroFin, succFin :: Constructor Raw.Raw Variable
zeroFin =
  Constructor indFin "fzero"
  [ ((), "n", "nat") ]
  [ ((), unsafeParseTerm "S n") ]
succFin =
  Constructor indFin "fsucc"
  [ ((), "n", "nat")
  , ((), "i", unsafeParseTerm "Fin n")
  ]
  [ ((), unsafeParseTerm "S n") ]

indVec :: Inductive Raw.Raw Variable
indVec =
  Inductive "Vec" [((), "A", Type U.Type)] [((), "size", "nat")] U.Type
  [ nilVec
  , consVec
  ]

nilVec, consVec :: Constructor Raw.Raw Variable
nilVec = Constructor indVec "vnil"  [] [((), Var Nothing "O")]
consVec =
  Constructor indVec "vcons"
  [ ((), "h", "A")
  , ((), "n", "nat")
  , ((), "t", unsafeParseTerm "Vec A n")
  ]
  [ ((), unsafeParseTerm "S n") ]

indEmpty :: Inductive Raw.Raw Variable
indEmpty =
  Inductive "False" [] [] U.Prop []

indUnit :: Inductive Raw.Raw Variable
indUnit =
  Inductive "unit" [] [] U.Set [ttUnit]

ttUnit :: Constructor Raw.Raw Variable
ttUnit = Constructor indUnit "tt" [] []

unitTests :: TestTree
unitTests = testGroup "Parsing.Inductive" $ []
  ++ [testCase "and"   $ indAnd   @?= SL.indAnd  ]
  ++ [testCase "bool"  $ indBool  @?= SL.indBool ]
  ++ [testCase "eq"    $ indEq    @?= SL.indEq   ]
  ++ [testCase "false" $ indEmpty @?= SL.indFalse]
  ++ [testCase "fin"   $ indFin   @?= SL.indFin  ]
  ++ [testCase "list"  $ indList  @?= SL.indList ]
  ++ [testCase "nat"   $ indNat   @?= SL.indNat  ]
  ++ [testCase "or"    $ indOr    @?= SL.indOr   ]
  ++ [testCase "unit"  $ indUnit  @?= SL.indUnit ]
  ++ [testCase "vec"   $ indVec   @?= SL.indVec  ]

test :: IO ()
test = defaultMain unitTests
