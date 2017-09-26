{-# language OverloadedStrings #-}
{-# language PartialTypeSignatures #-}

module StandardLibrary
  ( τId
  , τFlip
  , inductives
  , indBool
  , indEmpty
  , indFin
  , indList
  , indNat
  , indUnit
  , indVec
  , tId
  , tFlip
  ) where

-- import Control.Monad
-- import Control.Monad.Reader
-- import Data.Default

import Inductive.Inductive
import Parsing
-- import PrettyPrinting.PrettyPrintable
-- import PrettyPrinting.PrettyPrintableUnannotated
-- import PrettyPrinting.Utils
-- import Term.Binder
import Term.Raw                                  as Raw
import Term.Term
-- import Term.TypeChecked                          as Checked
import Text.Printf
-- import Typing.GlobalEnvironment
-- import Work

-- main :: IO ()
-- main = forM_ (unGlobalEnvironment stdlib) $ \ d ->
--   putStrLn $ prettyStrU d

-- addTerm ::
--   Variable -> (Raw.Term Variable, Raw.Type Variable) ->
--   GlobalEnvironment (Checked Variable) Variable ->
--   Either String (GlobalEnvironment (Checked Variable) Variable)
-- addTerm v (t, τ) ge =
--   case tc (checkF (toLocalContext ge) t τ id) of
--   Left  e ->
--     Left $
--     printf "Could not typecheck %s : %s at type %s\n%s"
--     (prettyStr v) (prettyStrU t) (prettyStrU τ) (prettyStrU e)
--   Right r -> Right $ addGlobalAssum (Binder (Just v), r) ge
--
-- traceTypeChecking ::
--   GlobalEnvironment (Checked Variable) Variable ->
--   TermX α Variable -> TermX ψ Variable -> IO ()
-- traceTypeChecking ge t τ = do
--   let trace = tcTrace stepTypeCheckerF (checkF (toLocalContext ge) t τ id)
--   forM_ trace $ \ item -> do
--     putStrLn $ doc2String $ runReader (prettyTypeCheckerF item) def
--
-- debug :: IO ()
-- debug = traceTypeChecking (GlobalEnvironment []) tId τId

-- stdlib :: GlobalEnvironment (Checked Variable) Variable
-- stdlib =
--   fromRight $ foldM (flip ($)) (GlobalEnvironment [])
--   [ addTerm "id" (tId, τId)
--   , addTerm "flip" (τFlip, tFlip)
--   , addInductives
--     [ inductiveBool
--     , inductiveNat
--     , inductiveList
--     , inductiveFin
--     , inductiveVec
--     , inductiveEmpty
--     , inductiveUnit
--     ]
--   ]
--   where
--     fromRight (Left  l) = error l
--     fromRight (Right r) = r

-- do not use `unsafeParseRaw` anywhere else!
unsafeParseRaw :: String -> Raw.Term Variable
unsafeParseRaw s =
  case parseMaybeTerm s of
    Nothing -> error $ printf "unsafeParseRaw: could not parse %s" s
    Just t  -> t

τId, tId :: Raw.Term Variable
τId = unsafeParseRaw "(T : Type) → T → T"
tId = unsafeParseRaw "λ T x . x"

τFlip, tFlip :: Raw.Term Variable
τFlip = unsafeParseRaw
  "(A B C : Type) → (A → B → C) → (B → A → C)"
tFlip = unsafeParseRaw "λ A B C f b a . f a b"

{-
inductive Bool : Type where
  true  : Bool
  false : Bool
-}
indBool :: Inductive Raw.Raw Variable
indBool =
  Inductive "bool" [] []
  [ trueBool
  , falseBool
  ]

trueBool, falseBool :: Constructor Raw.Raw Variable
trueBool  = Constructor indBool "true"  [] []
falseBool = Constructor indBool "false" [] []

{-
inductive nat : Type where
  zero : nat
  succ : (n : nat) → ℕ
-}
indNat :: Inductive Raw.Raw Variable
indNat =
  Inductive "nat" [] []
  [ zeroNat
  , succNat
  ]

zeroNat, succNat :: Constructor Raw.Raw Variable
zeroNat = Constructor indNat "O" [] []
succNat = Constructor indNat "S" [((), "n", "nat")] []

{-
inductive List (A : Type) : Type where
  nil  :                         List A
  cons : (x : A) (xs : List A) → List A
-}
indList :: Inductive Raw.Raw Variable
indList =
  Inductive "List" [((), "A", Type)] []
  [ nilList
  , consList
  ]

nilList, consList :: Constructor Raw.Raw Variable
nilList  = Constructor indList "nil"  [] []
consList = Constructor indList "cons"
    [ ((), "x", "A")
    , ((), "xs", unsafeParseRaw "List A")
    ]
    []

{-
inductive Fin : nat → Type where
  zero : {n : nat} → Fin (suc n)
  suc  : {n : nat} (i : Fin n) → Fin (suc n)
-}
indFin :: Inductive Raw.Raw Variable
indFin =
  Inductive "Fin" [] [((), "bound", "nat")]
  [ zeroFin
  , succFin
  ]

zeroFin, succFin :: Constructor Raw.Raw Variable
zeroFin =
  Constructor indFin "O"
  [ ((), "n", "nat") ]
  [ ((), unsafeParseRaw "S n") ]
succFin =
  Constructor indFin "succ"
  [ ((), "n", "nat")
  , ((), "i", unsafeParseRaw "Fin n")
  ]
  [ ((), unsafeParseRaw "S n") ]

{-
inductive Vec (A : Type) : nat → Type where
  nil  : Vec A zero
  cons : {n : nat} → (x : A) (xs : Vec A n) → Vec A (suc n)
-}
indVec :: Inductive Raw.Raw Variable
indVec =
  Inductive "Vec" [((), "A", Type)] [((), "size", "nat")]
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

{-
inductive ⊥ : Set where
-}
indEmpty :: Inductive Raw.Raw Variable
indEmpty =
  Inductive "⊥" [] [] []

{-
inductive ⊤ : Set where
  tt : ⊤
-}
indUnit :: Inductive Raw.Raw Variable
indUnit =
  Inductive "⊤" [] [] [ttUnit]

ttUnit :: Constructor Raw.Raw Variable
ttUnit = Constructor indUnit "tt" [] []

inductives :: [Inductive Raw.Raw Variable]
inductives =
  [ indBool
  , indNat
  , indList
  , indFin
  , indVec
  , indEmpty
  , indUnit
  ]
