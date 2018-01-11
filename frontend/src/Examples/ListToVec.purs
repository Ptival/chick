module Examples.ListToVec
       ( codeAfter
       , codeBefore
       )
       where

import Data.Monoid ((<>))

commonPrefix :: String
commonPrefix =
  """Inductive eq (A : Type) (x : A) : A → Prop :=
| eq_refl : eq A x x.

Inductive False : Prop :=.

Inductive or (A : Prop) (B : Prop) : Prop :=
| or_introl : A → or A B
| or_intror : B → or A B.

Inductive nat : Set :=
| O : nat
| S : nat → nat.

"""

codeBefore :: String
codeBefore =
  commonPrefix
  <> """Inductive list (A : Type) : Type :=
| nil : list A
| cons : A → list A → list A.
"""
  -- <> commonSuffix

codeAfter :: String
codeAfter =
  commonPrefix
  <> """Inductive Vec (A : Type) : ∀ (size : nat), Type :=
| vnil : Vec A O
| vcons : ∀ (h : A) (n : nat) (t : Vec A n), Vec A (S n).
"""
  --   <> commonSuffix

commonSuffix :: String
commonSuffix =
  """
Definition a_list : list nat := cons nat O (nil nat).

Definition length : ∀ (T : Type), list T → nat := λ T l, list_rect T (λ _, nat)
O (λ _ _ lt, S lt) l.

Definition length2 : ∀ (T : Type), list T → nat := λ _ l,
  match l with
  | nil _ => O
  | cons _ _ _ => S O
  end.

Definition hd : ∀ (A : Type), A → list A → A := λ A default l, list_rect A (λ _,
  A) default (λ x _ _, x) l.

Definition tl : ∀ (A : Type), list A → list A := λ A l, list_rect A (λ _, list
  A) (nil A) (λ _ x _, x) l.

Definition In : ∀ (A : Type), A → list A → Type := λ A a _, list_rect A (λ _,
  Type) False (λ _ b m, or (eq A b a) (In A a m)).

Fixpoint map : ∀ (A : Type), ∀ (B : Type), (A → B) → list A → Type := λ A B f l,

  match l with
  | nil _ => nil B
  | cons _ h t => cons B (f h) (map A B f t)
  end.
"""

initialCodePatched :: String
initialCodePatched = ""
