{-# language OverloadedStrings #-}
{-# language PartialTypeSignatures #-}

module StandardLibrary where

import Control.Monad
import Control.Monad.Reader
import Data.Default

import Inductive.Constructor
import Inductive.Inductive
import Parsing
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import PrettyPrinting.Utils
import Term.Binder
import Term.Raw                                  as Raw
import Term.Term
import Term.TypeChecked                          as Checked
import Term.Variable
import Text.Printf
import Typing.GlobalEnvironment
import Typing.Inductive
import Work

-- main :: IO ()
-- main = forM_ (unGlobalEnvironment stdlib) $ \ d ->
--   putStrLn $ prettyStrU d

addTerm ::
  Variable -> (Raw.Term Variable, Raw.Type Variable) ->
  GlobalEnvironment (Checked Variable) Variable ->
  Either String (GlobalEnvironment (Checked Variable) Variable)
addTerm v (t, τ) ge =
  case tc (checkF (toLocalContext ge) t τ id) of
  Left  e ->
    Left $
    printf "Could not typecheck %s : %s at type %s\n%s"
    (prettyStr v) (prettyStrU t) (prettyStrU τ) (prettyStrU e)
  Right r -> Right $ addGlobalAssum (Binder (Just v), r) ge

traceTypeChecking ::
  GlobalEnvironment (Checked Variable) Variable ->
  TermX ξ Variable -> TermX ψ Variable -> IO ()
traceTypeChecking ge t τ = do
  let trace = tcTrace stepTypeCheckerF (checkF (toLocalContext ge) t τ id)
  forM_ trace $ \ item -> do
    putStrLn $ doc2String $ runReader (prettyTypeCheckerF item) def

debug :: IO ()
debug = traceTypeChecking (GlobalEnvironment []) tId τId

stdlib :: GlobalEnvironment (Checked Variable) Variable
stdlib =
  fromRight $ foldM (flip ($)) (GlobalEnvironment [])
  [ addTerm "id" (tId, τId)
  , addTerm "flip" (τFlip, tFlip)
  , addInductives
    [ inductiveBool
    , inductiveNat
    , inductiveList
    , inductiveFin
    , inductiveVec
    , inductiveEmpty
    , inductiveUnit
    ]
  ]
  where
    fromRight (Left  l) = error l
    fromRight (Right r) = r

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
inductiveBool :: Inductive ξ Variable
inductiveBool =
  Inductive "𝔹" [] []
  [ Constructor "true"  [] []
  , Constructor "false" [] []
  ]

{-
inductive ℕ : Type where
  zero : ℕ
  succ : (n : ℕ) → ℕ
-}
inductiveNat :: Inductive () Variable
inductiveNat =
  Inductive "ℕ" [] []
  [ Constructor "zero"  [] []
  , Constructor "succ" [(Binder (Just "n"), Var "ℕ")] []
  ]

{-
inductive List (A : Type) : Type where
  nil  :                         List A
  cons : (x : A) (xs : List A) → List A
-}
inductiveList :: Inductive () Variable
inductiveList =
  Inductive "List" [("A", Type)] []
  [ Constructor "nil"  [] []
  , Constructor "cons"
    [ (Binder (Just "x"), Var "A")
    , (Binder (Just "xs"), App () (Var "List") (Var "A"))
    ]
    []
  ]

{-
inductive Fin : ℕ → Type where
  zero : {n : ℕ} → Fin (suc n)
  suc  : {n : ℕ} (i : Fin n) → Fin (suc n)
-}
inductiveFin :: Inductive () Variable
inductiveFin =
  Inductive "Fin" [] [Var "ℕ"]
  [ Constructor "zero"
    [ (Binder (Just "n"), Var "ℕ") ]
    [ App () (Var "succ") (Var "n") ]
  , Constructor "succ"
    [ (Binder (Just "n"), Var "ℕ")
    , (Binder (Just "i"), App () (Var "Fin") (Var "n"))
    ]
    [ App () (Var "succ") (Var "n") ]
  ]

{-
inductive Vec (A : Type) : ℕ → Type where
  nil  : Vec A zero
  cons : {n : ℕ} → (x : A) (xs : Vec A n) → Vec A (suc n)
-}
inductiveVec :: Inductive () Variable
inductiveVec =
  Inductive "Vec" [("A", Type)] [Var "ℕ"]
  [ Constructor "nil"  [] [Var "zero"]
  , Constructor "cons"
    [ (Binder (Just "n"), Var "ℕ")
    , (Binder (Just "x"), Var "A")
    , (Binder (Just "xs"), App () (App () (Var "Vec") (Var "A")) (Var "n"))
    ]
    [ App () (Var "succ") (Var "n") ]
  ]

{-
inductive ⊥ : Set where
-}
inductiveEmpty :: Inductive () Variable
inductiveEmpty =
  Inductive "⊥" [] [] []

{-
inductive ⊤ : Set where
  tt : ⊤
-}
inductiveUnit :: Inductive () Variable
inductiveUnit =
  Inductive "⊤" [] [] [Constructor "tt" [] []]
