From Coq Require Import
     EquivDec
     List
     Omega
     Relation_Operators
     String
.

From Chick Require Import
     Tactic.Atomic
     Goal
     LocalContext
     LocalDeclaration
.
Require Import Chick.Variable.

From HaysTac Require Import HaysTac.

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
(*| ExprVar   : forall (x : variable), expr*)
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
| TacAtom       : forall (a : atomic), tactic
| TacIdtac      : tactic
| TacFail       : forall (n : nat), tactic
| TacComp       : forall (e1 e2 : expr), tactic
| TacBranchComp : forall (e : expr) (es : list expr), tactic
| TacFirst      : forall (es : list expr), tactic
| TacProgress   : forall (e : expr), tactic
.

(*
Coercion ValTac : tactic >-> value.
Coercion ExprVal : value >-> expr.
 *)

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

Parameter subst_expr : variable -> expr -> expr -> expr.

Parameter subst_exprs : list variable -> list expr -> expr -> expr.

Inductive ArgsEval (g : goal) : list expr -> RARGS -> Type :=
.

Inductive ExprEval (g : goal) : expr -> RV -> Prop :=

(** Missing from the paper *)

(*
| VAR : forall v r,
    ??? ->
    ExprEval g (ExprVar v) r
 *)

(** Figure 7.4 *)

| VAL : forall v r,
    r = match v with
        | ValLam xs e => RVLam xs e
        | ValTac t    => RVTac t
        | ValInt n    => RVInt n
        end ->
    ExprEval g (ExprVal v) r

| FIX : forall x e rv,
    ExprEval g (subst_expr x (ExprFix x e) e) rv ->
    ExprEval g (ExprFix x e)                  rv

| APP1 : forall e es n,
    ExprEval g e              (RVFail n) ->
    ExprEval g (ExprApp e es) (RVFail n)

| APP2_Int : forall e es n,
    ExprEval g e              (RVInt n) ->
    ExprEval g (ExprApp e es) (RVFail 0)

| APP2_Tac : forall e es t,
    ExprEval g e              (RVTac t) ->
    ExprEval g (ExprApp e es) (RVFail 0)

| APP3 : forall e es xs e' n,
    ExprEval g e              (RVLam xs e') ->
    ArgsEval g es             (RARGSFail n) ->
    ExprEval g (ExprApp e es) (RVFail n)

| APP4 : forall e es xs e' vs xs1 xs2,
    ExprEval g e              (RVLam xs e') ->
    ArgsEval g es             (RARGSVals vs) ->
    List.length vs < List.length xs ->
    xs1 = List.firstn (List.length vs) xs ->
    xs2 = List.skipn  (List.length vs) xs ->
    ExprEval g (ExprApp e es) (RVLam xs2 (subst_exprs xs1 (List.map ExprVal vs) e))

where
"g ⊢ e ↓v r" := (ExprEval g e r)
.

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
      g ⊢ e                  ⇓  RXGoals gs ->
      g ⊢ TacFirst (e :: es) ↓x RXGoals gs

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

Scheme ExprExec_ind' := Minimality for ExprExec Sort Prop
  with TacExec_ind'  := Minimality for TacExec Sort Prop.

Combined Scheme Exec_ind from ExprExec_ind', TacExec_ind'.

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

Fixpoint S_v_size sv :=
  match sv with
  | Let _ _ _   s => S (S_v_size s)
  | App _ _     s => S (S_v_size s)
  | Args _ _    s => S (S_a_size s)
  | Pat _ _ _ _ s => S (S_v_size s)
  | EExec1 _    s => S (S_v_size s)
  | EExpr  _    s => S (S_x_size s)
  end
with S_a_size sa :=
       match sa with
       | Args1 _ _ _ s => S (S_v_size s)
       | Args2 _     s => S (S_a_size s)
       end
with S_x_size sx :=
       match sx with
       | Nil         => 0
       | Prog _    s => S (S_x_size s)
       | First _ _ s => S (S_x_size s)
       | Semi _    s => S (S_x_size s)
       | BSemi _   s => S (S_x_size s)
       | Seq1 _ _  s => S (S_x_size s)
       | Seq2 _    s => S (S_x_size s)
       | EExec2    s => S (S_v_size s)
  end.

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

  (** Figure 8.1 *)

  | E_v g (ExprVal (ValLam xs e))    s => A_v s (RVLam xs e)
  | E_v g (ExprVal (ValTac t))       s => A_v s (RVTac t)
  | E_v g (ExprVal (ValInt n))       s => A_v s (RVInt n)

  | E_v g (ExprApp e es)             s => E_v g e (App g es s)

  | E_v g (ExprLet x e1 e2)          s => E_v g e1 (Let g x e2 s)

  | E_v g (ExprMGoal [])             s => A_v s (RVFail 0)
  | E_v g (ExprMGoal ((p, e) :: cl)) s => E_pat g (start g p) e cl s

  | E_v g (ExprFix x e)              s => E_v g (subst_expr x (ExprFix x e) e) s

  | E_x g TacIdtac                   s => A_x s (RXGoals [g])

  | E_x g (TacFail n)                s => A_x s (RXFail n)

  | E_x g (TacProgress e)            s => E_ee g e (Prog g s)

  | E_x g (TacFirst [])              s => A_x s (RXFail 0)

  | E_x g (TacFirst (e :: es))       s => E_ee g e (First g es s)

  | E_x g (TacComp e1 e2)            s => E_ee g e1 (Semi e2 s)

  | E_x g (TacBranchComp e1 es)      s => E_ee g e1 (BSemi es s)

  | E_pat g m e cl                   s =>
    match next m with
    | None         => E_v g (ExprMGoal cl) s
    | Some (σ, m') => E_vx g (σ e) (Pat g m' e cl s)
    end

  | E_args g []                      s => A_args s (RARGSVals [])

  | E_args g (e :: es)               s => E_v g e (Args g es s)

  | E_seq [] []                      s => A_x s (RXGoals [])

  | E_seq (g :: gs) (e :: es)        s => E_ee g e (Seq1 gs es s)

  | E_vx g e                         s => E_v g e (EExec1 g s)

  | E_ee g e                         s => E_v g e (EExpr g s)

  (** Figure 8.2 *)

  | A_args (Args1 g xs e s) (RARGSFail n) => A_v s (RVFail n)

  | A_args (Args1 g xs e s) (RARGSVals vs) =>
    match Nat.compare (List.length vs) (List.length xs) with
    | Lt =>
      let lv := List.length vs in
      let xs1 := firstn lv xs in
      let xs2 := firstn lv xs in
      A_v s (RVLam xs2 (subst_exprs xs1 (List.map ExprVal vs) e))
    | Eq => E_v g (subst_exprs xs (List.map ExprVal vs) e) s
    | Gt =>
      let lx := List.length xs in
      let vs1 := firstn lx vs in
      let vs2 := firstn lx vs in
      E_v g (subst_exprs xs (List.map ExprVal vs1) e) s
    end

  | A_args (Args2 v s) (RARGSFail n) => A_args s (RARGSFail n)

  | A_args (Args2 v s) (RARGSVals vs) => A_args s (RARGSVals (v :: vs))

  (** Figure 8.3 *)

  | A_x (Prog g s) (RXFail n) => A_x s (RXFail n)

  | A_x (Prog g s) (RXGoals gs) =>
    A_x s (
          match gs with
          | [g'] =>
            if g == g'
            then RXFail 0
            else RXGoals gs
          | _ => RXGoals gs
          end
        )

  | A_x (First g es s) (RXFail 0)     => E_x g (TacFirst es) s

  | A_x (First g es s) (RXFail (S n)) => A_x s (RXFail n)

  | A_x (First g es s) (RXGoals gs)   => A_x s (RXGoals gs)

  | A_x (Semi e s) (RXFail n)   => A_x s (RXFail n)

  | A_x (Semi e s) (RXGoals gs) => E_seq gs (List.repeat e (List.length gs)) s

  | A_x (BSemi es s) (RXFail n)   => A_x s (RXFail n)

  | A_x (BSemi es s) (RXGoals gs) =>
    if List.length gs == List.length es
    then E_seq gs es s
    else A_x s (RXFail 0)

  | A_x (Seq1 gs es s) (RXFail n)   => A_x s (RXFail n)

  | A_x (Seq1 gs es s) (RXGoals gs') => E_seq gs es (Seq2 gs' s)

  | A_x (Seq2 gs s) (RXFail n)    => A_x s (RXFail n)

  | A_x (Seq2 gs s) (RXGoals gs') => A_x s (RXGoals (gs ++ gs'))

  | A_x (EExec2 s) r => A_v s r

  (** Figure 8.4 *)

  | A_v (Let g x e2 s) (RVGoals gs) => A_v s (RVFail 0)

  | A_v (Let g x e2 s) (RVFail n)   => A_v s (RVFail n)

  | A_v (Let g x e2 s) (RVLam xs e) => E_v g (subst_expr x (ExprVal (ValLam xs e)) e2) s

  | A_v (Let g x e2 s) (RVInt n)    => E_v g (subst_expr x (ExprVal (ValInt n)) e2) s

  | A_v (Let g x e2 s) (RVTac t)    => E_v g (subst_expr x (ExprVal (ValTac t)) e2) s

  | A_v (App g es s) (RVFail n)   => A_v s (RVFail n)

  | A_v (App g es s) (RVLam xs e) => E_args g es (Args1 g xs e s)

  | A_v (App g es s) _            => A_v s (RVFail 0)

  | A_v (Args g es s) (RVLam xs e) => E_args g es (Args2 (ValLam xs e) s)
  | A_v (Args g es s) (RVInt n)    => E_args g es (Args2 (ValInt n)    s)
  | A_v (Args g es s) (RVTac t)    => E_args g es (Args2 (ValTac t)    s)

  | A_v (Args g es s) (RVGoals gs) => A_args s (RARGSFail 0)

  | A_v (Args g es s) (RVFail n)   => A_args s (RARGSFail n)

  | A_v (Pat g m e cl s) (RVLam xs e')  => A_v s (ValLam xs e')
  | A_v (Pat g m e cl s) (RVInt n)      => A_v s (ValInt n)
  | A_v (Pat g m e cl s) (RVTac t)      => A_v s (ValTac t)

  | A_v (Pat g m e cl s) (RVGoals gs)   => A_v s (RVGoals gs)

  | A_v (Pat g m e cl s) (RVFail 0)     => E_pat g m e cl s

  | A_v (Pat g m e cl s) (RVFail (S n)) => A_v s (RVFail n)

  | A_v (EExec1 g s) (RVFail n)   => A_v s (RVFail n)

  | A_v (EExec1 g s) (RVGoals gs) => A_v s (RVGoals gs)

  | A_v (EExec1 g s) (RVInt n)    => A_v s (RVInt n)

  | A_v (EExec1 g s) (RVLam xs e) => A_v s (RVLam xs e)

  | A_v (EExec1 g s) (RVTac t)    => E_x g t (EExec2 s)

  | A_v (EExpr g s) (RVFail n)   => A_x s (RXFail n)

  | A_v (EExpr g s) (RVGoals gs) => A_x s (RXGoals gs)

  | A_v (EExpr g s) (RVInt n)    => A_x s (RXFail 0)

  | A_v (EExpr g s) (RVLam xs e) => A_x s (RXFail 0)

  | A_v (EExpr g s) (RVTac t)    => E_x g t s

  (* Final configuration steps to itself *)

  | A_x Nil r => A_x Nil r

  (* Missing from the paper: *)
  (*
  | E_v _ (ExprVar _) _ => TODO
   *)

  | E_x _ (TacAtom _) _ => TODO

  (* by construction, I believe this never happens: *)
  | E_seq [] (_ :: _) _ => TODO
  | E_seq (_ :: _) [] _ => TODO

  end.

Fixpoint step (n : nat) (c : configuration) : configuration :=
  match n with
  | O => c
  | S n' => step n' (stepConfiguration c)
  end.

Notation "a ⇒ b" := (step 1 a = b) (at level 50).

(*
Definition step_star := clos_refl_trans _ (fun a b => a ⇒ b).
 *)

Definition step_star (a b : configuration) : Prop := exists n, step n a = b.

Notation "a ⇒* b" := (step_star a b) (at level 50).

Lemma decompose_step:
  forall (n : nat) (cStart cMiddle cFinal : configuration),
    step n cStart = cMiddle ->
    forall x : nat,
      step x cMiddle = cFinal ->
      step (n + x) cStart = cFinal.
Proof.
  intro.
  on nat induction'; intros.
  simpl in *.
  - now on @eq inversion'.
  - simpl in *.
    now find_eapply; eauto.
Qed.

Global Instance Reflexive_step_star : Reflexive step_star.
Proof.
  repeat intro.
  now (exists 0).
Qed.

Global Instance Transitive_step_star : Transitive step_star.
Proof.
  repeat intro.
  unfold step_star in *.
  do 2 on @ex destruct'.
  eexists (_ + _).
  erewrite decompose_step.
  - reflexivity.
  - eassumption.
  - eassumption.
Qed.

Theorem decompose_star :
  forall n cStart cMiddle cFinal,
    step n cStart = cMiddle ->
    cMiddle ⇒* cFinal ->
    cStart ⇒* cFinal.
Proof.
  intros.
  unfold step_star in *.
  on @ex destruct'.
  eexists (_ + _).
  eapply decompose_step; eauto.
Qed.

Ltac step n := eapply (decompose_star n); [ reflexivity | simpl ].

Tactic Notation "on_context" open_constr(c) tactic(tac) :=
  match goal with [ H : context [c] |- _ ] => tac H end.

(*
Lemma stack_comes_back :
  forall s r r',
    exists n, step (S n) (A_v s r) = A_v s r'.
Proof.
  intro.
  on S_v induction'; intros.
  - simpl.
    break_match_in_goal.
    edestruct IHs.
    eexists (S _).
    apply H.

Qed.

Lemma stack_decreases :
  forall n s r s' r',
    step n (A_v s r) = A_v s' r' ->
    S_v_size s >= S_v_size s'.
Proof with (try solve [on step find_apply_in; simpl in *; omega]).
  intro.
  on_head nat ltac:(fun n => induction n using lt_wf_ind); intros.
  on_head nat destruct'.
  { on_head @eq inversion'. omega. }
  on step simpl'.
  break_match_in_hyp; subst; simpl.
  - break_match_in_hyp; subst...
    + next.
      on step simpl'.
      break_match_in_hyp.
      * break_match_in_hyp...
      * next; simpl in *.
        break_match_in_hyp; subst.
        { break_match_in_hyp... }
        { next; simpl in *.
          break_match_in_hyp; subst.
          { break_match_in_hyp; subst.
            { on_head nat destruct'.
              { apply H in H0. simpl in H0. on step inversion'. simpl.
            apply H in H0.

Qed.
*)

Theorem abstractMachineCorrect_val :
  forall (g : goal) (e : expr) (r : RV),
    g ⊢ e ↓v r -> (forall s, E_v g e s ⇒* A_v s r).
Proof.
  do 4 intro.
  on_head ExprEval induction';  intros.
  - step 1.
    now on_head value destruct'; subst.
  - now step 1.
  - step 1.
    find_rewrite_r.
    now step 1.
  - step 1.
    find_rewrite_r.
    now step 1.
  - step 1.
    find_rewrite_r.
    now step 1.
  - step 1.
    find_rewrite_r.
    now step 1.
  - step 1.
    find_rewrite_r.
    now step 1.
Qed.

(*
{
    revert r H.
    unfold step_star.
    induction e; intros.

    {
      on_head value destruct'.
      {
        econstructor.
        next.
        { now inversion H; constructor. }
        {
          on_head S_v destruct'.
          next.
          break_match_in H.
          break_match_in H.
          next.
          inversion H.
          apply f_equal with (f := S_v_size) in H1.
          simpl in H1. omega.
          admit.
          admit.
          admit.
          admit.
          admit.
          admit.
          admit.
          admit.
          admit.
          admit.
          admit.
          admit.
        }
      }
      { admit. }
      { admit. }
    }
    { admit. }
    { admit. }
    { admit. }
    { admit. }
  }
Admitted.
 *)

Theorem abstractMachineCorrect_exec :
  forall g,
    (forall e r, g ⊢ e ⇓ r -> forall s, E_ee g e s ⇒* A_x s r)
    /\
    (forall t r, g ⊢ t ↓x r -> forall s, E_x g t s ⇒* A_x s r)
.
Proof.
  intro.
  apply (
      Exec_ind _
        (fun e r => forall s, E_ee g e s ⇒* A_x s r)
        (fun t r => forall s, E_x g t s ⇒* A_x s r)
    ); intros.
  - step 1.
    on ExprEval ltac:(in_eapply abstractMachineCorrect_val).
    on_head step_star rewrite_r.
    now step 1.
  - step 1.
    on ExprEval ltac:(in_eapply abstractMachineCorrect_val).
    on_head step_star rewrite_r.
    now step 1.
  - step 1.
    on ExprEval ltac:(in_eapply abstractMachineCorrect_val).
    on_head step_star rewrite_r.
    now step 1.
  - step 1.
    on ExprEval ltac:(in_eapply abstractMachineCorrect_val).
    on_head step_star rewrite_r.
    now step 1.
  - step 1.
    on ExprEval ltac:(in_eapply abstractMachineCorrect_val).
    on_head step_star rewrite_r.
    now step 1.
  - now step 1.
  - now step 1.
  - now step 1.
  - step 1.
    find_rewrite_r.
    now step 1.
  - step 1.
    find_rewrite_r.
    now step 1.
  - step 1.
    find_rewrite_r.
    now step 1.
  - step 1.
    find_rewrite_r.
    now step 1.
Qed.

Ltac next := on_head nat destruct'; [ try discriminate | ]; on step simpl'.

Theorem abstractMachineCorrect_exec' :
  forall g,
    (forall e r, E_ee g e Nil ⇒* A_x Nil r -> g ⊢ e ⇓ r)
    /\
    (forall t r, E_x g t Nil ⇒* A_x Nil r -> g ⊢ t ↓x r)
.
Proof.
  split; intros.
  {
    on_head RX generalize_dependent'.
    on_head expr induction'; intros.
    - on step_star destruct'.
      next.
      next.
      break_match_in_hyp; subst.
      + next.
        next.
        * on @eq inversion'.
          constructor.
          constructor.
      simpl in H.

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
  (* | TCComma (RXGoals gs) ??? => ??? *)
  (* The paper does not make sense there *)

  (* seq_app *)
  (*| TCComma (RXGoals gs) ??? => RXGoals (gs ++ ???)*)
  (* The paper does not make sense there *)

  (* seq_gs *)
  | TCComma (RXGoals gs) es => TCSeq (SeqExprs gs es)

  (* seq_cons *)

  | _ => TODO

  end.
