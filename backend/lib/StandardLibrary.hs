{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module StandardLibrary
  ( τId,
    τFlip,
    inductives,
    indAnd,
    indBool,
    indFalse,
    indEq,
    indFin,
    indList,
    indNat,
    indOr,
    indProd,
    indUnit,
    indVec,
    terms,
    tId,
    tFlip,
  )
where

import Data.String.QQ (s)
import Inductive.Inductive (Inductive)
import Parsing.Unsafe (unsafeParseInductive, unsafeParseTerm)
import Term.Raw as Raw (Raw, Term)
import Term.Variable (Variable)

τId, tId :: Raw.Term Variable
τId = unsafeParseTerm "(T : Type) → T → T"
tId = unsafeParseTerm "λ T x, x"

τFlip, tFlip :: Raw.Term Variable
τFlip =
  unsafeParseTerm
    "(A B C : Type) → (A → B → C) → (B → A → C)"
tFlip = unsafeParseTerm "λ A B C f b a, f a b"

terms :: [Raw.Term Variable]
terms =
  [ τId,
    tId,
    τFlip,
    tFlip
  ]

indAnd :: Inductive Raw.Raw Variable
indAnd =
  unsafeParseInductive
    [s|
Inductive and (A B : Prop) : Prop :=
| conj : ∀ (a : A) (b : B), and A B
|]

indBool :: Inductive Raw.Raw Variable
indBool =
  unsafeParseInductive
    [s|
Inductive bool : Set :=
| true : bool
| false : bool
|]

indEq :: Inductive Raw.Raw Variable
indEq =
  unsafeParseInductive . unlines $
    [ "Inductive eq (A : Type) (x : A) : ∀ (other : A), Prop :=",
      "| eq_refl : eq A x x"
    ]

indProd :: Inductive Raw.Raw Variable
indProd =
  unsafeParseInductive
    [s|
Inductive prod (A : Type) (B : Type) : Type :=
| pair : ∀ (a : A) (b : B), prod A B
|]

indNat :: Inductive Raw.Raw Variable
indNat =
  unsafeParseInductive
    [s|
Inductive nat : Set :=
| O : nat
| S : ∀ (n : nat), nat
|]

indOr :: Inductive Raw.Raw Variable
indOr =
  unsafeParseInductive . unlines $
    [ "Inductive or (A B : Prop) : Prop :=",
      "| or_introl : ∀ (a : A), or A B",
      "| or_intror : ∀ (b : B), or A B"
    ]

indList :: Inductive Raw.Raw Variable
indList =
  unsafeParseInductive . unlines $
    [ "Inductive list (A : Type) : Type :=",
      "| nil : list A",
      "| cons : ∀ (x : A) (xs : list A), list A"
    ]

indFin :: Inductive Raw.Raw Variable
indFin =
  unsafeParseInductive . unlines $
    [ "Inductive Fin : ∀ (bound : nat), Set :=",
      "| fzero : ∀ (n : nat), Fin (S n)",
      "| fsucc : ∀ (n : nat) (i : Fin n), Fin (S n)"
    ]

indVec :: Inductive Raw.Raw Variable
indVec =
  unsafeParseInductive
    [s|
Inductive Vec (A : Type) : ∀ (size : nat), Type :=
| vnil : Vec A O
| vcons : ∀ (h : A) (n : nat) (t : Vec A n), Vec A (S n)
|]

indFalse :: Inductive Raw.Raw Variable
indFalse =
  unsafeParseInductive
    [s|
Inductive False : Prop :=
|]

indUnit :: Inductive Raw.Raw Variable
indUnit =
  unsafeParseInductive
    [s|
Inductive unit : Set :=
| tt : unit
|]

inductives :: [Inductive Raw.Raw Variable]
inductives =
  [ indAnd,
    indBool,
    indEq,
    indFalse,
    indFin,
    indList,
    indNat,
    indOr,
    indProd,
    indUnit,
    indVec
  ]
