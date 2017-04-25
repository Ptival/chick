From Coq Require Import
     List
.

From Chick Require Import
     Fresh
     Goal
     LocalContext
     Unify
.
Require Import Chick.Variable.

From Chick.CoreLtac.Syntax Require Import
     Atomic
.

From HaysTac Require Import HaysTac.

Import ListNotations.

Fixpoint piListRev pis φ :=
  match pis with
  | [] => φ
  | (pi :: pis) =>
    piListRev pis (mkPi None pi φ)
  end.

Definition piList pis φ := piListRev (List.rev pis) φ.

Parameter matcher : Type.
Parameter pattern : Type.

Inductive expr : Type :=
| ExprVal   : forall (v : value), expr
(*| ExprVar   : forall (x : variable), expr*)
| ExprApp   : forall (e : expr) (es : list expr), expr
| ExprLet   : forall (x : variable) (e1 e2 : expr), expr
(*
| ExprMGoal : forall (cl : list (pattern * expr)), expr
*)
| ExprFix   : forall (x : variable) (e : expr), expr

with
value : Type :=
| ValInt : forall (n : nat), value
| ValLam : forall (l : lam), value
| ValTac : forall (t : tactic), value

with
tactic : Type :=
| TacAtom     : forall (a : atomic), tactic
| TacIdtac    : tactic
| TacFail     : forall (n : nat), tactic
| TacSemi     : forall (e1 e2 : expr), tactic
| TacBranch   : forall (e : expr) (es : list expr), tactic
| TacFirst    : forall (es : list expr), tactic
| TacProgress : forall (e : expr), tactic

with
lam : Type :=
| Lam : forall (xs : list variable) (e : expr), lam
.

Notation "λ xs . e" := (Lam xs e) (at level 50).

Coercion ExprVal : value  >-> expr.
Coercion ValLam  : lam    >-> value.
Coercion ValTac  : tactic >-> value.
Coercion ValInt  : nat      >-> value.
Coercion TacAtom : atomic >-> tactic.

(*
Scheme expr_ind'   := Induction for expr   Sort Prop
  with value_ind'  := Induction for value  Sort Prop
  with tactic_ind' := Induction for tactic Sort Prop
.

Combined Scheme ltac_ind from expr_ind', value_ind', tactic_ind'.
 *)

Section ltac_ind.

  Variables (E : expr -> Prop) (V : value -> Prop) (T : tactic -> Prop) (L : list expr -> Prop).

  Hypotheses
    (H__VAL : forall v, V v -> E v)
    (H__APP : forall e, E e -> forall es, L es -> E (ExprApp e es))
    (H__LET : forall x e1, E e1 -> forall e2, E e2 -> E (ExprLet x e1 e2))
    (*f2 : forall cl, E (ExprMGoal cl)*)
    (H__FIX : forall x e, E e -> E (ExprFix x e))
    (H__INT : forall n : nat, V n) (H__LAM : forall l : lam, V l)
    (H__TAC : forall t, T t -> V t) (H__ATOM : forall a : atomic, T a)
    (H__IDTAC : T TacIdtac) (H__FAIL : forall n : nat, T (TacFail n))
    (H__SEMI : forall e1 : expr, E e1 -> forall e2 : expr, E e2 -> T (TacSemi e1 e2))
    (H__BRANCH : forall e : expr, E e -> forall es : list expr, L es -> T (TacBranch e es))
    (H__FIRST : forall es, L es -> T (TacFirst es))
    (H__PROGRESS : forall e : expr, E e -> T (TacProgress e))
    (L__NIL : L [])
    (L__CONS : forall e, E e -> forall l, L l -> L (e :: l))
  .

  Fixpoint expr_ind' (e : expr) : E e :=
    match e as e0 return (E e0) with
    | ExprVal v => H__VAL v (value_ind' v)
    | ExprApp e0 es =>
      H__APP e0 (expr_ind' e0) es
         ((fix list_expr_ind l : L l :=
             match l as x return L x with
             | [] => L__NIL
             | h::t => L__CONS _ (expr_ind' _) _ (list_expr_ind _)
             end
          ) es)
    | ExprLet x e1 e2 => H__LET x e1 (expr_ind' e1) e2 (expr_ind' e2)
    (*| ExprMGoal cl => f2 cl*)
    | ExprFix x e0 => H__FIX x e0 (expr_ind' e0)
    end
  with
  value_ind' (v : value) : V v :=
    match v as v0 return (V v0) with
    | ValInt n => H__INT n
    | ValLam l => H__LAM l
    | ValTac t => H__TAC t (tactic_ind' t)
    end
  with
  tactic_ind' (t : tactic) : T t :=
    match t as t0 return (T t0) with
    | TacAtom a => H__ATOM a
    | TacIdtac => H__IDTAC
    | TacFail n => H__FAIL n
    | TacSemi e1 e2 => H__SEMI e1 (expr_ind' e1) e2 (expr_ind' e2)
    | TacBranch e es =>
      H__BRANCH e (expr_ind' e) es
              ((fix list_expr_ind l : L l :=
                  match l as x return L x with
                  | [] => L__NIL
                  | h::t => L__CONS _ (expr_ind' _) _ (list_expr_ind _)
                  end
               ) es)
    | TacFirst es =>
      H__FIRST es
          ((fix list_expr_ind l : L l :=
              match l as x return L x with
              | [] => L__NIL
              | h::t => L__CONS _ (expr_ind' _) _ (list_expr_ind _)
              end
           ) es)
    | TacProgress e => H__PROGRESS e (expr_ind' e)
    end
  .

End ltac_ind.

Combined Scheme ltac_ind from expr_ind', value_ind', tactic_ind'.

Parameter subst_expr : variable -> expr -> expr -> expr.

Parameter subst_exprs : list variable -> list expr -> expr -> expr.
