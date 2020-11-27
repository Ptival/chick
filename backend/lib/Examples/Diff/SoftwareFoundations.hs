{-# LANGUAGE QuasiQuotes #-}

module Examples.Diff.SoftwareFoundations
  ( scriptBefore,
    scriptAfter,
  )
where

import Data.String.QQ (s)
import Language (Language (Chick))
import Parsing.Unsafe (unsafeParseScript)
import PrettyPrinting.Chick ()
import PrettyPrinting.PrettyPrintableUnannotated
  ( PrettyPrintableUnannotated (prettyStrU),
  )
import Script (Script)
import qualified StandardLibrary as SL
import qualified Term.Raw as Raw
import Term.Term (Variable)

commonPrefix :: String
commonPrefix =
  prettyStrU @ 'Chick SL.indBool ++ ".\n"
    ++ prettyStrU @ 'Chick SL.indNat
    ++ ".\n"
    ++ [s|

Fixpoint ifthenelse : ∀ (T : Type), bool → T → T → T := λ T i t e,
  match i with
  | true  => t
  | false => e
  end.

Fixpoint beq_nat : nat → nat → bool := λ n m,
  match n with
  | O => match m with
         | O   => true
         | S _ => false
         end
  | S n1 => match m with
            | O    => false
            | S m1 => beq_nat n1 m1
            end
  end.

Inductive id : Type :=
| Id : ∀ (id : nat), id
.

Definition beq_id : id → id → bool := λ id1 id2,
  match id1 with
  | Id n1 => match id2 with
             | Id n2 => beq_nat n1 n2
             end
  end.

Inductive ty : Type :=
| TArrow : ty → ty → ty
| TNat : ty
| TUnit : ty
| TProd : ty → ty → ty
| TSum : ty → ty → ty
| TList : ty → ty.

|]

codeBefore :: String
codeBefore =
  commonPrefix
    ++ [s|
Inductive tm : Type :=
  (* pure STLC *)
  | tvar : id → tm
  | tapp : tm → tm → tm
  | tabs : id → ty → ∀ (t0 : tm), tm.
  |]
    ++ commonSuffix

codeAfter :: String
codeAfter =
  commonPrefix
    ++ [s|
Inductive tm : Type :=
  (* pure STLC *)
  | tvar : id → tm
  | tapp : tm → tm → tm
  | tabs : id → ty → tm → ∀ (t0 : tm), tm → tm
  (* numbers *)
  | tnat : nat → tm
  | tsucc : tm → tm → tm
  | tpred : tm → tm
  | tmult : tm → tm → tm
  | tif0 : tm → tm → tm → tm.
  |]
    ++ commonSuffix

commonSuffix :: String
commonSuffix =
  [s|
Fixpoint subst : id → tm → tm → tm := λ x s t ,
  match t with
  | tvar y =>
      ifthenelse (beq_id x y) s t
  | tabs y T t1 =>
      tabs y T (ifthenelse (beq_id x y) t1 (subst x s t1))
  | tapp t1 t2 =>
      tapp t1 t2
  end.
  |]

scriptAfter :: Script Raw.Raw Variable
scriptAfter = unsafeParseScript codeAfter

scriptBefore :: Script Raw.Raw Variable
scriptBefore = unsafeParseScript codeBefore
