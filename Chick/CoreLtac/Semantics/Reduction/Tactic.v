From Coq Require Import
     EquivDec
     List
     Omega
     Operators_Properties
     Relation_Operators
.

From Chick Require Import
     Goal
     LocalContext
     LocalDeclaration
     ReservedNotations
     TODO
.
Require Import Chick.Variable.

From Chick.CoreLtac Require Import
     Results
     Semantics.Natural
     Semantics.SmallStepOperational.Untyped
     Semantics.Agreement.NaturalAndSmallStepOperationalUntyped.Atomic
     Syntax
.

From HaysTac Require Import HaysTac.

Import ListNotations.

Inductive exprclosure : Type :=
| ECResult : forall (r : RV), exprclosure
| ECExpr   : forall (g : goal) (e : expr), exprclosure
| ECApp    : forall (c : exprclosure) (cs : listclosure), exprclosure
| ECLet    : forall (x : variable) (c1 c2 : exprclosure), exprclosure
| ECMGoal  : forall (m : matcher) (c : exprclosure)
               (cl : list (pattern * expr)), exprclosure
| ECMEval  : forall (m : matcher) (pat : (goal * expr * exprclosure))
               (cl : list (pattern * expr)), exprclosure
| ECEval   : forall (ct : tacticclosure), exprclosure
| ECExec1  : forall (g : goal) (c : exprclosure), exprclosure

with
listclosure : Type :=
| LCNil : listclosure
| LCExprs : forall (g : goal) (es : list expr), listclosure
| LCCons : forall (c : exprclosure) (cs : listclosure), listclosure
| LCFail : forall (f : fail), listclosure

with
tacticclosure : Type :=
| TCResult   : forall (rx : RX), tacticclosure
| TCExec     : forall (g : goal) (e : expr), tacticclosure
| TCTac      : forall (g : goal) (t : tactic), tacticclosure
| TCSemi     : forall (ct : tacticclosure) (e : expr), tacticclosure
| TCBSemi    : forall (ct : tacticclosure) (es : list expr), tacticclosure
| TCComma    : forall (ct : tacticclosure) (es : list expr), tacticclosure
| TCSeq      : forall (s : sequence), tacticclosure
| TCFirst    : forall (ct : tacticclosure) (c : list exprclosure), tacticclosure
| TCProgress : forall (g : goal) (ct : tacticclosure), tacticclosure

with
sequence : Type :=
| SeqNil   : sequence
| SeqExprs : forall (gs : list goal) (es : list expr), sequence
| SeqCons  : forall (ct : tacticclosure) (s : sequence), sequence
.

Coercion ECResult : RV >-> exprclosure.
Coercion TCResult : RX >-> tacticclosure.

Parameter subst_exprclosure : variable -> exprclosure -> exprclosure -> exprclosure.

Definition ecstep (e : exprclosure) : exprclosure :=
  match e with

  (* beta_bot *)
  | ECApp (ECExpr g _) (LCFail n) => Fail__RV n

  (* prop_app *)
  | ECExpr g (ExprApp e es) => ECApp (ECExpr g e) (LCExprs g es)

  (* app_l_bot *)
  | ECApp (Fail__RV n) cs => Fail__RV n

  (* app_l_nval *)
  | ECApp _ cs => Fail 0

  (* prop_let *)
  | ECExpr g (ExprLet x e1 e2) => ECLet x (ECExpr g e1) (ECExpr g e2)

  (* let_gs *)
  | ECLet x (Goals__RV gs) c => Fail 0

  (* let_bot *)
  | ECLet x (Fail__RV n) c => Fail__RV n

  (* let_v *)
  | ECLet x v c => subst_exprclosure x v c

  (* mg_nil *)
  (* | ECExpr g (ExprMGoal []) => Fail__RV 0 *)

  (* prop_mg *)
  (* | ECExpr g (ExprMGoal ((p, e) :: cl)) => ECMGoal (start g p) (ECExpr g e) cl *)

  (* mg_none *)
  (* | ECMGoal m (ECExpr g e) cl => *)
  (*   match next m with *)
  (*   | None         => ECExpr g (ExprMGoal cl) *)
  (*   | Some (σ, m') => ECMEval m' (g, e, ECExec1 g (ECExpr g (σ e))) cl *)
  (*   end *)

  (* mg_gs *)
  | ECMEval m' (g, e, Goals__RV gs) cl => Goals__RV gs

  (* mg_bot0 *)
  | ECMEval m' (g, e, Fail 0) cl => ECMGoal m' (ECExpr g e) cl

  (* mg_botS *)
  | ECMEval m' (g, e, Fail (S n)) cl => Fail n

  (* mg_val *)
  | ECMEval m' (g, e, v) cl => v

  (* fix *)
  | ECExpr g (ExprFix x e) => ECExpr g (subst_expr x (ExprFix x e) e)

  (* exec1_tac *)
  | ECExec1 g (ValTac t) => ECEval (TCTac g t)

  (* exec1_res + exec1_val *)
  | ECExec1 g (ECResult v)   => v

  (* eval_res *)
  | ECEval (TCResult rx) => ECResult rx

  (* goal_val *)
  | ECExpr g (ExprVal v) => ECResult v

  (* what about ? *)

  | ECResult res => TODO
  | ECMGoal _ _ _ => TODO
  | ECEval _ => TODO
  | ECExec1 _ _ => TODO
  | ECExpr _ (ExprVar _) => TODO

  end
.

(*
  | TCResult : RX -> tacticclosure
  | TCExec : goal -> expr -> tacticclosure
  | TCTac : goal -> tactic -> tacticclosure
  | TCSemi : tacticclosure -> expr -> tacticclosure
  | TCBSemi : tacticclosure -> list expr -> tacticclosure
  | TCComma : tacticclosure -> list expr -> tacticclosure
  | TCSeq : sequence -> tacticclosure
  | TCFirst : tacticclosure -> list exprclosure -> tacticclosure
  | TCProgress : goal -> tacticclosure -> tacticclosure
*)

Parameter goal_eq_dec : forall (g1 g2 : goal), {g1 = g2} + {g1 <> g2}.

Definition tcstep (tc : tacticclosure) : tacticclosure :=
  match tc with

  (* idtac *)
  | TCTac g TacIdtac => Goals__RX [g]

  (* fail *)
  | TCTac g (TacFail n) => Fail n

  (* prop_progress *)
  | TCTac g (TacProgress e) => TCProgress g (TCExec g e)

  (* progress_eq + progress_neq *)
  | TCProgress g (Goals__RX gs) =>
    if List.list_eq_dec goal_eq_dec [g] gs
    then Fail 0
    else Goals__RX gs

  (* progress_bot *)
  | TCProgress g (Fail__RX n) => Fail__RX n

  (* semi_bot_l *)
  | TCSemi (Fail__RX n) _ => Fail__RX n

  (* semi_gs *)
  | TCSemi (Goals__RX gs) e => TCSeq (SeqExprs gs (repeat e (List.length gs)))

  (* bsemi_bot_l *)
  | TCBSemi (Fail__RX n) _ => Fail__RX n

  (* bsemi_gs *)
  | TCBSemi (Goals__RX gs) es =>
    if Nat.eq_dec (length gs) (length es)
    then Fail 0
    else TCSeq (SeqExprs gs es)

  (* seq_bot_l *)
  | TCComma (Fail__RX n) _ => Fail__RX n

  (* seq_bot_r *)
  (* | TCComma (Goals__RX gs) ??? => ??? *)
  (* The paper does not make sense there *)

  (* seq_app *)
  (*| TCComma (Goals__RX gs) ??? => Goals__RX (gs ++ ???)*)
  (* The paper does not make sense there *)

  (* seq_gs *)
  | TCComma (Goals__RX gs) es => TCSeq (SeqExprs gs es)

  (* seq_cons *)

  | _ => TODO

  end.
