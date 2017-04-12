From Coq Require Import
     List
     String
.

From Chick Require Import
     Tactic.Atomic
     Goal
     LocalContext
     LocalDeclaration
.
Require Import Chick.Variable.

Import ListNotations.

Parameter Fresh : variable -> localContext -> Prop.

Parameter Unify : term -> term -> Prop.

Fixpoint piListRev pis φ :=
  match pis with
  | [] => φ
  | (pi :: pis) =>
    piListRev pis (mkPi None pi φ)
  end.

Definition piList pis φ := piListRev (List.rev pis) φ.

Reserved Notation "Γ ⊢ x ⇓ v" (at level 50).

Inductive AtomicExec : goal -> atomic -> option (list goal) -> Prop :=

| AssumptionOK :
    forall Γ φ,
      TypeInLocalContext φ Γ ->
      Γ ÷ φ ⊢ AtomAssumption ⇓ Some []

| IntroOK :
    forall b x Γ ψ φ,
      Fresh x Γ ->
      Γ ÷ (mkPi b ψ φ) ⊢ AtomIntro x ⇓ Some [(LocalAssum x φ :: Γ) ÷ ψ]

| ApplyOK :
    forall H Γ φ ψ pis,
      InLocalContext (LocalAssum H φ) Γ ->
      Unify ψ (piList pis φ) ->
      Γ ÷ φ ⊢ AtomApply H ⇓ Some (List.map (Goal Γ) pis)

where "Γ ⊢ x ⇓ v" := (AtomicExec Γ x v)
.

Parameter matcher : Type.
Parameter pattern : Type.

Inductive expr : Type :=
| ExprVal   : forall (v : value), expr
| ExprVar   : forall (x : variable), expr
| ExprApp   : forall (e : expr) (es : list expr), expr
| ExprLet   : forall (x : variable) (e1 e2 : expr), expr
| ExprMGoal : forall (cl : list (pattern * expr)), expr
| ExprFix   : forall (x : variable) (e : expr), expr

with
value : Type :=
| ValLam : forall (xs : list variable) (e : expr), value
| ValTac : forall (t : tactic), value
| ValInt : forall (n : nat), value

with
tactic : Type :=
| TacAtom     : forall (a : atomic), tactic
| TacIdtac    : tactic
| TacFail     : forall (n : nat), tactic
| TacComp     : forall (e1 e2 : expr), tactic
| TacBranch   : forall (e : expr) (es : list expr), tactic
| TacFirst    : forall (es : list expr), tactic
| TacProgress : forall (e : expr), tactic
.

Inductive RV :=
| RVFail  : forall (n : nat), RV
| RVGoals : forall (gs : list goal), RV
| RVLam   : forall (xs : list variable) (e : expr), RV
| RVInt   : forall (n : nat), RV
| RVTac   : forall (t : tactic), RV
.

Definition coerce_value_to_RV (v : value) : RV :=
  match v with
  | ValLam xs e => RVLam xs e
  | ValInt n    => RVInt n
  | ValTac t    => RVTac t
  end.

Coercion coerce_value_to_RV : value >-> RV.

Inductive RVX :=
| RVXFail  : forall (n : nat), RVX
| RVXGoals : forall (gs : list goal), RVX
| RVXLam   : forall (xs : list variable) (e : expr), RVX
| RVXInt   : forall (n : nat), RVX
.

Definition coerce_RVX_to_RV (rvx : RVX) : RV :=
  match rvx with
  | RVXFail n   => RVFail n
  | RVXGoals gs => RVGoals gs
  | RVXLam xs e => RVLam xs e
  | RVXInt n    => RVInt n
  end.
Coercion coerce_RVX_to_RV : RVX >-> RV.

Inductive RX :=
| RXFail  : forall (n : nat), RX
| RXGoals : forall (gs : list goal), RX
.

Definition coerce_RX_to_RVX (rx : RX) : RVX :=
  match rx with
  | RXFail n   => RVXFail n
  | RXGoals gs => RVXGoals gs
  end.
Coercion coerce_RX_to_RVX : RX >-> RVX.

Inductive RARGS :=
| RARGSFail : forall (n : nat), RARGS
| RARGSVals : forall (vs : list value), RARGS
.

Reserved Notation "g ⊢ e ↓v gs" (at level 50).
Reserved Notation "g ⊢ e ↓vx gs" (at level 50).
Reserved Notation "g ⊢ e ↓x gs" (at level 50).
Reserved Notation "g ⊢ e ↓seq gs" (at level 50).
Reserved Notation "g ⊢ e ⇓ r" (at level 50).

Inductive ExprEval (g : goal) (e : expr) : RV -> Type :=
where
"g ⊢ e ↓v r" := (ExprEval g e r)
.

Require Import List.
Import ListNotations.

Inductive TacSeqExec (g : goal) (e : expr) : RX -> Prop :=
where
"g ⊢ e ↓seq r" := (TacSeqExec g e r)
.

Inductive ExprExec (g : goal) : expr -> RX -> Prop :=
| EEX1 :
    forall e gs,
      g ⊢ e ↓v (RVGoals gs) ->
      g ⊢ e ⇓  (RXGoals gs)
| EEX2 :
    forall e t rx,
      g ⊢ e ↓v (RVTac t) ->
      g ⊢ t ↓x rx ->
      g ⊢ e ⇓  rx
| EEX3 :
    forall e n,
      g ⊢ e ↓v (RVFail n) ->
      g ⊢ e ⇓  (RXFail n)
| EEX4_1 :
    forall e n,
      g ⊢ e ↓v (RVInt n) ->
      g ⊢ e ⇓  (RXFail 0)
| EEX4_2 :
    forall e xs e',
      g ⊢ e ↓v (RVLam xs e') ->
      g ⊢ e ⇓  (RXFail 0)
where

"g ⊢ e ⇓ r" := (ExprExec g e r)

with

TacExec (g : goal) : tactic -> RX -> Prop :=

| IDTAC :
    g ⊢ TacIdtac ↓x RXGoals [g]

| FAIL :
    forall n,
      g ⊢ TacFail n ↓x RXFail n

| FIRST1 :
    g ⊢ TacFirst [] ↓x RXFail 0

| FIRST2 :
    forall e es gs,
      g ⊢ e                  ⇓  gs ->
      g ⊢ TacFirst (e :: es) ↓x gs

| FIRST3 :
    forall e es n,
      g ⊢ e                  ⇓  RXFail (S n) ->
      g ⊢ TacFirst (e :: es) ↓x RXFail n

| FIRST4 :
    forall e es rx,
      g ⊢ e                  ⇓  RXFail 0 ->
      g ⊢ TacFirst es        ↓x rx     ->
      g ⊢ TacFirst (e :: es) ↓x rx

| PROGR1 :
    forall e n,
      g ⊢ e             ⇓  RXFail n ->
      g ⊢ TacProgress e ↓x RXFail n

(* TODO *)

where

"g ⊢ e ↓x r" := (TacExec g e r)
.

Inductive ExtendedExprEval (g : goal) (e : expr) : RVX -> Prop :=
| EEV1 :
    forall gs,
      g ⊢ e ↓v  (RVGoals gs) ->
      g ⊢ e ↓vx (RVXGoals gs)
| EEV2 :
    forall t rx,
      g ⊢ e ↓v  (RVTac t) ->
      g ⊢ t ↓x  rx       ->
      g ⊢ e ↓vx rx
| EEV3 :
    forall n,
      g ⊢ e ↓v  (RVFail n) ->
      g ⊢ e ↓vx (RVXFail n)
| EEV4_1 :
    forall n,
      g ⊢ e ↓v  (RVInt n) ->
      g ⊢ e ↓vx (RVXInt n)
| EEV4_2 :
    forall xs e',
      g ⊢ e ↓v  (RVLam xs e') ->
      g ⊢ e ↓vx (RVXLam xs e')
where
"g ⊢ e ↓vx r" := (ExtendedExprEval g e r)
.



(*** ABSTRACT MACHINE ***)

Inductive S_v : Type :=
| Let    : forall (g : goal) (x : variable) (e : expr) (s : S_v), S_v
| App    : forall (g : goal) (es : list expr) (s : S_v), S_v
| Args   : forall (g : goal) (es : list expr) (s : S_a), S_v
| Pat    : forall (g : goal) (m : matcher) (e : expr) (cl : list (pattern * expr))
                  (s : S_v), S_v
| EExec1 : forall (g : goal) (s : S_v), S_v
| EExpr  : forall (g : goal) (s : S_x), S_v

with

S_a : Type :=
| Args1 : forall (g : goal) (xs : list variable) (e : expr) (s : S_v), S_a
| Args2 : forall (v : value) (s : S_a), S_a

with

S_x : Type :=
| Nil    : S_x
| Prog   : forall (g : goal) (s : S_x), S_x
| First  : forall (g : goal) (es : list expr) (s : S_x), S_x
| Semi   : forall (e : expr) (s : S_x), S_x
| BSemi  : forall (es : list expr) (s : S_x), S_x
| Seq1   : forall (gs : list goal) (es : list expr) (s : S_x), S_x
| Seq2   : forall (gs : list goal) (s : S_x), S_x
| EExec2 : forall (s : S_v), S_x

.

Inductive configuration : Type :=
| E_v    : forall (g : goal) (e : expr) (s : S_v), configuration
    (* I think there's a typo in the thesis here, is says A_v takes a RX *)
| A_v    : forall (s : S_v) (r : RV), configuration
| E_x    : forall (g : goal) (t : tactic) (s : S_x), configuration
| A_x    : forall (s : S_x) (r : RX), configuration
| E_ee   : forall (g : goal) (e : expr) (s : S_x), configuration
| E_vx   : forall (g : goal) (e : expr) (s : S_v), configuration
| E_pat  : forall (g : goal) (m : matcher) (e : expr) (cl : list (pattern * expr))
                 (s : S_v), configuration
| E_args : forall (g : goal) (es : list expr) (s : S_a), configuration
| A_args : forall (s : S_a) (r : RARGS), configuration
| E_seq  : forall (g : list goal) (es : list expr) (s : S_x), configuration
.

Definition initialconfiguration (g : goal) (e : expr) :=
  E_ee g e Nil.

Axiom TODO : forall {T}, T.

Parameter start : goal -> pattern -> matcher.
Parameter next : matcher -> option ((expr -> expr) * matcher).

Definition stepConfiguration (c : configuration) : configuration :=
  match c with

  | E_v g (ExprVal (ValLam xs e))    s => A_v s (RVLam xs e)
  | E_v g (ExprVal (ValTac t))       s => A_v s (RVTac t)
  | E_v g (ExprVal (ValInt n))       s => A_v s (RVInt n)

  | E_v g (ExprApp e es)             s => E_v g e (App g es s)

  | E_v g (ExprLet x e1 e2)          s => E_v g e1 (Let g x e2 s)

  | E_v g (ExprMGoal [])             s => A_v s (RVFail 0)
  | E_v g (ExprMGoal ((p, e) :: cl)) s => E_pat g (start g p) e cl s

  | _ => TODO
  end.

Parameter step : nat -> configuration -> configuration.

Notation "a ⇒ b" := (step 1 a = b) (at level 50).

Notation "a ⇒* b" := (exists n, step n a = b) (at level 50).

Theorem abstractMachineCorrect :
  forall (g : goal) (e : expr) (r : RX),
    g ⊢ e ⇓ r <-> E_ee g e Nil ⇒* A_x Nil r.
Proof.
  intros g e r.
  split; intros H.
  {
    admit.
  }
  {
    admit.
  }
Admitted.

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
| LCFail : forall (n : nat), listclosure

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

Parameter subst_expr : variable -> expr -> expr -> expr.

Parameter subst_exprclosure : variable -> exprclosure -> exprclosure -> exprclosure.

Definition ecstep (e : exprclosure) : exprclosure :=
  match e with

  (* beta_bot *)
  | ECApp (ECExpr g _) (LCFail n) => RVFail n

  (* prop_app *)
  | ECExpr g (ExprApp e es) => ECApp (ECExpr g e) (LCExprs g es)

  (* app_l_bot *)
  | ECApp (RVFail n) cs => RVFail n

  (* app_l_nval *)
  | ECApp _ cs => RVFail 0

  (* prop_let *)
  | ECExpr g (ExprLet x e1 e2) => ECLet x (ECExpr g e1) (ECExpr g e2)

  (* let_gs *)
  | ECLet x (RVGoals gs) c => RVFail 0

  (* let_bot *)
  | ECLet x (RVFail n) c => RVFail n

  (* let_v *)
  | ECLet x v c => subst_exprclosure x v c

  (* mg_nil *)
  | ECExpr g (ExprMGoal []) => RVFail 0

  (* prop_mg *)
  | ECExpr g (ExprMGoal ((p, e) :: cl)) => ECMGoal (start g p) (ECExpr g e) cl

  (* mg_none *)
  | ECMGoal m (ECExpr g e) cl =>
    match next m with
    | None         => ECExpr g (ExprMGoal cl)
    | Some (σ, m') => ECMEval m' (g, e, ECExec1 g (ECExpr g (σ e))) cl
    end

  (* mg_gs *)
  | ECMEval m' (g, e, RVGoals gs) cl => RVGoals gs

  (* mg_bot0 *)
  | ECMEval m' (g, e, RVFail 0) cl => ECMGoal m' (ECExpr g e) cl

  (* mg_botS *)
  | ECMEval m' (g, e, RVFail (S n)) cl => RVFail n

  (* mg_val *)
  | ECMEval m' (g, e, v) cl => v

  (* fix *)
  | ECExpr g (ExprFix x e) => ECExpr g (subst_expr x (ExprFix x e) e)

  (* exec1_tac *)
  | ECExec1 g (RVTac t) => ECEval (TCTac g t)

  (* exec1_res + exec1_val *)
  | ECExec1 g (ECResult v)   => v

  (* eval_res *)
  | ECEval (TCResult rx) => ECResult rx

  (* goal_val *)
  | ECExpr g (ExprVal v) => ECResult v

  (* what about ? *)

  | ECResult res => TODO
  | ECExpr _ (ExprVar _) => TODO
  | ECMGoal _ _ _ => TODO
  | ECEval _ => TODO
  | ECExec1 _ _ => TODO

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

Require Import Coq.Arith.PeanoNat.

Definition tcstep (tc : tacticclosure) : tacticclosure :=
  match tc with

  (* idtac *)
  | TCTac g TacIdtac => RXGoals [g]

  (* fail *)
  | TCTac g (TacFail n) => RXFail n

  (* prop_progress *)
  | TCTac g (TacProgress e) => TCProgress g (TCExec g e)

  (* progress_eq + progress_neq *)
  | TCProgress g (RXGoals gs) =>
    if list_eq_dec goal_eq_dec [g] gs
    then RXFail 0
    else RXGoals gs

  (* progress_bot *)
  | TCProgress g (RXFail n) => RXFail n

  (* semi_bot_l *)
  | TCSemi (RXFail n) _ => RXFail n

  (* semi_gs *)
  | TCSemi (RXGoals gs) e => TCSeq (SeqExprs gs (repeat e (length gs)))

  (* bsemi_bot_l *)
  | TCBSemi (RXFail n) _ => RXFail n

  (* bsemi_gs *)
  | TCBSemi (RXGoals gs) es =>
    if Nat.eq_dec (length gs) (length es)
    then RXFail 0
    else TCSeq (SeqExprs gs es)

  (* seq_bot_l *)
  | TCComma (RXFail n) _ => RXFail n

  (* seq_bot_r *)
  (*| TCComma (RXGoals gs) (RXFail n) => RXFail n*)

  (* seq_app *)
  (*| TCComma (RXGoals gs) (RXGoals gs') => RXGoals (gs ++ gs')*)

  (* seq_gs *)
  | TCComma (RXGoals gs) es => TCSeq (SeqExprs gs es)

  (* seq_cons *)


  | _ => TODO

  end.
