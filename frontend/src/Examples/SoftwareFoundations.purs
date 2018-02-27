module Examples.SoftwareFoundations
       ( codeAfter
       , codeBefore
       )
       where

import Data.Monoid ((<>))

commonPrefix :: String
commonPrefix =
  """
Inductive bool : Set :=
| true : bool
| false : bool
.

Inductive nat : Set :=
| O : nat
| S : ∀ (n : nat), nat
.

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

"""

codeBefore :: String
codeBefore =
  commonPrefix
  <> """
Inductive tm : Type :=
  (* pure STLC *)
  | tvar : id → tm
  | tapp : tm → tm → tm
  | tabs : id → ty → tm → tm.
"""
  <> commonSuffix

codeAfter :: String
codeAfter =
  commonPrefix
  <> """
Inductive tm : Type :=
  (* pure STLC *)
  | tvar : id → tm
  | tapp : tm → tm → tm
  | tabs : id → ty → tm → tm
  (* numbers *)
  | tnat : nat → tm
  | tsucc : tm → tm
  | tpred : tm → tm
  | tmult : tm → tm → tm
  | tif0 : tm → tm → tm → tm.
"""
  <> commonSuffix

commonSuffix :: String
commonSuffix =
  """
Fixpoint subst : id → tm → tm → tm := λ x s t ,
  match t with
  | tvar y =>
      ifthenelse (beq_id x y) s t
  | tabs y T t1 =>
      tabs y T (ifthenelse (beq_id x y) t1 (subst x s t1))
  | tapp t1 t2 =>
      tapp t1 t2
  end.
"""

initialCodePatched :: String
initialCodePatched = ""
