{-# LANGUAGE OverloadedStrings #-}

module Parsing.Inductive.Test
  ( test
  , unitTests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           Inductive.Inductive
import           Parsing
import qualified StandardLibrary as SL
import qualified Term.Raw as Raw
import           Term.Term
import qualified Term.Universe as U

-- do not use `unsafeParseRaw` anywhere else!
unsafeParseRaw :: String -> Raw.Term Variable
unsafeParseRaw s =
  case parseMaybeTerm s of
    Nothing -> error $ printf "unsafeParseRaw: could not parse %s" s
    Just t  -> t

indBool :: Inductive Raw.Raw Variable
indBool =
  Inductive "bool" [] [] U.Set
  [ trueBool
  , falseBool
  ]

trueBool, falseBool :: Constructor Raw.Raw Variable
trueBool  = Constructor indBool "true"  [] []
falseBool = Constructor indBool "false" [] []

indNat :: Inductive Raw.Raw Variable
indNat =
  Inductive "nat" [] [] U.Set
  [ zeroNat
  , succNat
  ]

zeroNat, succNat :: Constructor Raw.Raw Variable
zeroNat = Constructor indNat "O" [] []
succNat = Constructor indNat "S" [((), "n", "nat")] []

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
    , ((), "xs", unsafeParseRaw "list A")
    ]
    []

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
  [ ((), unsafeParseRaw "S n") ]
succFin =
  Constructor indFin "fsucc"
  [ ((), "n", "nat")
  , ((), "i", unsafeParseRaw "Fin n")
  ]
  [ ((), unsafeParseRaw "S n") ]

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
  , ((), "t", unsafeParseRaw "Vec A n")
  ]
  [ ((), unsafeParseRaw "S n") ]

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
  ++ [testCase "bool"  $ indBool  @?= SL.indBool ]
  ++ [testCase "empty" $ indEmpty @?= SL.indEmpty]
  ++ [testCase "fin"   $ indFin   @?= SL.indFin  ]
  ++ [testCase "list"  $ indList  @?= SL.indList ]
  ++ [testCase "nat"   $ indNat   @?= SL.indNat  ]
  ++ [testCase "unit"  $ indUnit  @?= SL.indUnit ]
  ++ [testCase "vec"   $ indVec   @?= SL.indVec  ]

test :: IO ()
test = defaultMain unitTests
