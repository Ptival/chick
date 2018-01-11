module Examples.SoftwareFoundations
       ( codeAfter
       , codeBefore
       )
       where

import Data.Monoid ((<>))

commonPrefix :: String
commonPrefix =
  """
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
  -- <> commonSuffix

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
  -- <> commonSuffix

commonSuffix :: String
commonSuffix =
  """
Fixpoint subst (x:id) (s:tm) (t:tm) : tm :=
  match t with
  | tvar y ⇒
      if beq_id x y then s else t
  | tabs y T t1 ⇒
      tabs y T (if beq_id x y then t1 else (subst x s t1))
  | tapp t1 t2 ⇒
      tapp (subst x s t1) (subst x s t2)
  end.
"""

initialCodePatched :: String
initialCodePatched = ""
