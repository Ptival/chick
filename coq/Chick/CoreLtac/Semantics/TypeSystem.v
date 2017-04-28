From Coq Require Import List.

From Chick Require Import
     Goal
.
Require Import Chick.Variable.

From Chick.CoreLtac Require Import
     Results
     Syntax
     Syntax.SmallStepOperationalAbstractMachine
.

Import ListNotations.

Inductive valueType : Type :=
| TypeNat : valueType
| TypeTac : valueType
| TypeArr : forall (τ1 : valueType) (τ2 : generalType), valueType
with
generalType : Type :=
| TypeVal : forall (τ : valueType), generalType
| TypeGoal : generalType
(*
| TypeNil : generalType
| TypeCons : forall (h : valueType) (t : generalType), generalType
*)
.

Coercion TypeVal : valueType >-> generalType.

Definition force (t : generalType) : generalType :=
  match t with
  | TypeTac => TypeGoal
  | _ => t
  end.

Reserved Notation "Γ ⊢ a ∷ b" (at level 50).
Reserved Notation "Γ ⊢ a ∷rargs b" (at level 50).
Reserved Notation "Γ ⊢ a ∷rv b" (at level 50).
Reserved Notation "Γ ⊢ a ∷rx b" (at level 50).
Reserved Notation "Γ ⊢ a ∷∷ b" (at level 50).
Reserved Notation "S ⇐a τ" (at level 50).
Reserved Notation "S ⇐v τ" (at level 50).
Reserved Notation "S ⇐x τ" (at level 50).
Reserved Notation "c 'OK'" (at level 50).

Definition typingContext := list (variable * generalType).

Definition arrows (νs : list valueType) (τ : generalType) : generalType :=
  List.fold_right (fun x a => TypeArr x a) τ νs.

Definition InExtendedContext
           (Γ : typingContext)
           (Γ' : typingContext)
           (P : typingContext -> Prop) : Prop :=
  forall (Γ'' : typingContext),
    (forall p, In p Γ -> In p Γ') ->
    (forall p, In p Γ -> In p Γ'') ->
    P Γ''.

Inductive Typing : typingContext -> expr -> generalType -> Prop :=

| VAR :
    forall Γ (x : variable) (ν : valueType),
      List.In (x, TypeVal ν) Γ ->
      Γ ⊢ x ∷ ν

| LIT :
    forall Γ n,
      Γ ⊢ ⎡n⎤ ∷ TypeNat

| ABS :
    forall Γ (xs : list variable) (νs : list valueType) e τ τe,
      InExtendedContext
        Γ
        (combine xs (map TypeVal νs))
        (fun Γ => Γ ⊢ e ∷ τe) ->
      τ = arrows νs τe ->
      Γ ⊢ λ xs → e ∷ τ

| APP :
    forall Γ e es νs τ τe,
      τe = arrows νs τ ->
      Γ ⊢ e ∷ τe ->
      (forall ei νi,
          In (ei, νi) (combine es νs) ->
          Γ ⊢ ei ∷ νi
      ) ->
      Γ ⊢ ExprApp e es ∷ τ

| LET :
    forall Γ (x : variable) e1 e2 ν τ,
      Γ ⊢ e1 ∷ ν ->
      InExtendedContext Γ [(x, ν)] (fun Γ => Γ ⊢ e2 ∷ τ) ->
      Γ ⊢ ExprLet x e1 e2 ∷ τ

(* missing MGoal *)

| FIX :
    forall Γ x e τ,
      InExtendedContext Γ [(x, τ)] (fun Γ => Γ ⊢ e ∷ τ) ->
      Γ ⊢ ExprFix x e ∷ τ

| IDTAC :
    forall Γ,
      Γ ⊢ TacIdtac ∷ TypeTac

| FAIL :
    forall Γ n,
      Γ ⊢ TacFail n ∷ TypeTac

| SEMI :
    forall Γ e1 e2,
      Tactic e1 ->
      Tactic e2 ->
      Γ ⊢ TacSemi e1 e2 ∷ TypeTac

| BRANCH :
    forall Γ e1 es,
      Tactic e1 ->
      (forall e, In e es -> Tactic e) ->
      Γ ⊢ TacBranch e1 es ∷ TypeTac

| FIRST :
    forall Γ es,
      (forall e, In e es -> Tactic e) ->
      Γ ⊢ TacFirst es ∷ TypeTac

| PROGRESS :
    forall Γ e,
      Tactic e ->
      Γ ⊢ TacProgress e ∷ TypeTac

(* TODO: others *)

where

"Γ ⊢ a ∷ b" := (Typing Γ a b)

with TypingRX : typingContext -> RX -> generalType -> Prop :=

| GOALS__RX :
    forall Γ (gs : goals),
      Γ ⊢ gs ∷rx TypeGoal

| BOT__RX :
    forall Γ n τ,
      Γ ⊢ Fail n ∷rx τ

where
"Γ ⊢ a ∷rx b" := (TypingRX Γ a b)

with TypingRV : typingContext -> RV -> generalType -> Prop :=

| GOALS__RV :
    forall Γ (gs : goals),
      Γ ⊢ gs ∷rv TypeGoal

| BOT__RV :
    forall Γ n τ,
      Γ ⊢ Fail n ∷rv τ

where
"Γ ⊢ a ∷rv b" := (TypingRV Γ a b)

(* whether this is the correct interpretation is very unclear from the
   paper... *)
with TypingRARGS : typingContext -> RARGS -> list valueType -> Prop :=

| BOT__RARGS :
    forall Γ n τ,
      Γ ⊢ Fail n ∷rargs τ

| VALS__RARGS :
    forall Γ (vs : values) νs,
      Γ ⊢ map ExprVal vs ∷∷ νs ->
      Γ ⊢ vs ∷rargs νs

where
"Γ ⊢ a ∷rargs b" := (TypingRARGS Γ a b)

with
TypingList : typingContext -> list expr -> list valueType -> Prop :=

| NIL :
    forall Γ,
      Γ ⊢ [] ∷∷ []

| CONS :
    forall Γ v vs ν νs,
      Γ ⊢ v :: vs ∷∷ (ν :: νs)

where
"Γ ⊢ a ∷∷ b" := (TypingList Γ a b)

with
Tactic : expr -> Prop :=

| TACTIC :
    forall Γ e τ,
      Γ ⊢ e ∷ τ ->
      force τ = TypeGoal->
      Tactic e

with
ValidStack__v : S__v -> generalType -> Prop :=

| LET__v :
    forall g x e Sv ν τ,
      Sv ⇐v τ ->
      [(x, ν)] ⊢ e ∷ τ ->
      Let g x e Sv ⇐v ν

| APP__v :
    forall g es Sv (νs : list valueType) τ,
      Sv ⇐v τ ->
      [] ⊢ es ∷∷ νs ->
      App g es Sv ⇐v arrows νs τ

| ARGS__v :
    forall g es Sa ν νs,
      Sa ⇐a (ν :: νs) ->
      [] ⊢ es ∷∷ νs ->
      Args g es Sa ⇐v ν

(* TODO
| PAT__v :*)

| EEXEC__v :
    forall g Sv τ,
      Sv ⇐v force τ ->
      EExec1 g Sv ⇐v τ

| EE1__v :
    forall g Sx,
      Sx ⇐x TypeGoal ->
      EExpr g Sx ⇐v TypeGoal

| EE2__v :
    forall g Sx,
      Sx ⇐x TypeGoal ->
      EExpr g Sx ⇐v TypeTac

where
"S ⇐v τ" := (ValidStack__v S τ)

with
ValidStack__a : S__a -> list valueType -> Prop :=

| ARGS1__v :
    forall g (l : lam) Sv νs τ,
      Sv ⇐v τ ->
      [] ⊢ l ∷ arrows νs τ ->
      Args1 g l Sv ⇐a νs

| ARGS2__v :
    forall (v : value) Sa ν νs,
      Sa ⇐a (ν :: νs) ->
      [] ⊢ v ∷ ν ->
      Args2 v Sa ⇐a νs

where
"S ⇐a τ" := (ValidStack__a S τ)

with
ValidStack__x : S__x -> generalType -> Prop :=

| NIL__x :
    Nil ⇐x TypeGoal

| PROG__x :
    forall g Sx,
      Sx ⇐x TypeGoal ->
      Prog g Sx ⇐x TypeGoal

| FIRST__x :
    forall g es Sx,
      Sx ⇐x TypeGoal ->
      (forall e, In e es -> Tactic e) ->
      First g es Sx ⇐x TypeGoal

| SEMI__x :
    forall e Sx,
      Sx ⇐x TypeGoal ->
      Tactic e ->
      Semi e Sx ⇐x TypeGoal

| BSEMI__x :
    forall es Sx,
      Sx ⇐x TypeGoal ->
      (forall e, In e es -> Tactic e) ->
      BSemi es Sx ⇐x TypeGoal

| SEQ1__x :
    forall gs es Sx,
      Sx ⇐x TypeGoal ->
      (forall e, In e es -> Tactic e) ->
      List.length gs = List.length es ->
      Seq1 gs es Sx ⇐x TypeGoal

| SEQ2__x :
    forall gs Sx,
      Sx ⇐x TypeGoal ->
      Seq2 gs Sx ⇐x TypeGoal

| EEXEC2__x :
    forall Sv,
      Sv ⇐v TypeGoal ->
      EExec2 Sv ⇐x TypeGoal

where
"S ⇐x τ" := (ValidStack__x S τ)

with
ValidConfiguration : configuration -> Prop :=

| EVAL__X :
    forall g (e : tactic) Sx,
      Sx ⇐x TypeGoal ->
      [] ⊢ e ∷ TypeTac ->
      E__x g e Sx OK

| APPLY__X :
    forall Sx rx,
      Sx ⇐x TypeGoal ->
      A__x Sx rx OK

| EVAL__V :
    forall g (e : tactic) τ Sv,
      Sv ⇐v τ ->
      [] ⊢ e ∷ τ ->
      E__v g e Sv OK

| APPLY__V :
    forall Sv (r__v : RV) τ,
      Sv ⇐v τ ->
      [] ⊢ r__v ∷rv τ ->
      A__v Sv r__v OK

| EVAL__EE :
    forall g e Sx,
      Sx ⇐x TypeGoal ->
      Tactic e ->
      E__ee g e Sx OK

| EVAL__EEXT :
    forall g e Sv τ,
      Sv ⇐v force τ ->
      [] ⊢ e ∷ τ ->
      E__vx g e Sv OK

(* TODO
| EVAL__PAT :
 *)

| EVAL__ARGS :
    forall g es Sa νs,
      Sa ⇐a νs ->
      [] ⊢ es ∷∷ νs ->
      E__args g es Sa OK

| APPLY__ARGS :
    forall Sa (r__args : values) νs,
      Sa ⇐a νs ->
      [] ⊢ r__args ∷rargs νs ->
      A__args Sa r__args OK

| EVAL__SEQ :
    forall gs es Sx,
      Sx ⇐x TypeGoal ->
      (forall e, In e es -> Tactic e) ->
      List.length gs = List.length es ->
      E__seq gs es Sx OK

where
"c 'OK'" := (ValidConfiguration c)

.
