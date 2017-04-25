From Coq Require Import
     EquivDec
     List
     Omega
     Operators_Properties
     Relation_Operators
     String
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
     Semantics.SmallStepOperational.Untyped.Atomic
     Syntax.Tactic
.

From HaysTac Require Import HaysTac.

Import ListNotations.

(*** ABSTRACT MACHINE ***)

Inductive S__v : Type :=
| Let    : forall (g : goal) (x : variable) (e : expr) (s : S__v), S__v
| App    : forall (g : goal) (es : list expr) (s : S__v), S__v
| Args   : forall (g : goal) (es : list expr) (s : S__a), S__v
| Pat    : forall (g : goal) (m : matcher) (e : expr) (cl : list (pattern * expr))
             (s : S__v), S__v
| EExec1 : forall (g : goal) (s : S__v), S__v
| EExpr  : forall (g : goal) (s : S__x), S__v

with

S__a : Type :=
| Args1 : forall (g : goal) (l : lam) (s : S__v), S__a
| Args2 : forall (v : value) (s : S__a), S__a

with

S__x : Type :=
| Nil    : S__x
| Prog   : forall (g : goal) (s : S__x), S__x
| First  : forall (g : goal) (es : list expr) (s : S__x), S__x
| Semi   : forall (e : expr) (s : S__x), S__x
| BSemi  : forall (es : list expr) (s : S__x), S__x
| Seq1   : forall (gs : list goal) (es : list expr) (s : S__x), S__x
| Seq2   : forall (gs : list goal) (s : S__x), S__x
| EExec2 : forall (s : S__v), S__x

.

Fixpoint size_S__v sv :=
  match sv with
  | Let _ _ _   s => S (size_S__v s)
  | App _ _     s => S (size_S__v s)
  | Args _ _    s => S (size_S__a s)
  | Pat _ _ _ _ s => S (size_S__v s)
  | EExec1 _    s => S (size_S__v s)
  | EExpr  _    s => S (size_S__x s)
  end

with
size_S__a sa :=
  match sa with
  | Args1 _ _ s => S (size_S__v s)
  | Args2 _   s => S (size_S__a s)
  end

with
size_S__x sx :=
  match sx with
  | Nil         => 0
  | Prog _    s => S (size_S__x s)
  | First _ _ s => S (size_S__x s)
  | Semi _    s => S (size_S__x s)
  | BSemi _   s => S (size_S__x s)
  | Seq1 _ _  s => S (size_S__x s)
  | Seq2 _    s => S (size_S__x s)
  | EExec2    s => S (size_S__v s)
  end.

Inductive configuration : Type :=
| E__v    : forall (g : goal) (e : expr) (s : S__v), configuration
    (* I think there's a typo in the thesis here, is says A__v takes a RX *)
| A__v    : forall (s : S__v) (r : RV), configuration
| E__x    : forall (g : goal) (t : tactic) (s : S__x), configuration
| A__x    : forall (s : S__x) (r : RX), configuration
| E__ee   : forall (g : goal) (e : expr) (s : S__x), configuration
| E__vx   : forall (g : goal) (e : expr) (s : S__v), configuration
| E__pat  : forall (g : goal) (m : matcher) (e : expr) (cl : list (pattern * expr))
                 (s : S__v), configuration
| E__args : forall (g : goal) (es : list expr) (s : S__a), configuration
| A__args : forall (s : S__a) (r : RARGS), configuration
| E__seq  : forall (g : list goal) (es : list expr) (s : S__x), configuration
.

Definition initialconfiguration (g : goal) (e : expr) :=
  E__ee g e Nil.

Parameter start : goal -> pattern -> matcher.
Parameter next : matcher -> option ((expr -> expr) * matcher).

Definition stepConfiguration (c : configuration) : configuration :=
  match c with

  (** Figure 8.1 *)

  | E__v g (ExprVal v)                s => A__v s v
  | E__v g (ExprApp e es)             s => E__v g e (App g es s)
  | E__v g (ExprLet x e1 e2)          s => E__v g e1 (Let g x e2 s)
  (*
  | E__v g (ExprMGoal [])             s => A__v s (Fail 0)
  | E__v g (ExprMGoal ((p, e) :: cl)) s => E__pat g (start g p) e cl s
   *)
  | E__v g (ExprFix x e)              s => E__v g (subst_expr x (ExprFix x e) e) s
  | E__x g TacIdtac                   s => A__x s ([g] : goals)
  | E__x g (TacFail n)                s => A__x s (Fail n)
  | E__x g (TacProgress e)            s => E__ee g e (Prog g s)
  | E__x g (TacFirst [])              s => A__x s (Fail 0)
  | E__x g (TacFirst (e :: es))       s => E__ee g e (First g es s)
  | E__x g (TacSemi e1 e2)            s => E__ee g e1 (Semi e2 s)
  | E__x g (TacBranch e1 es)          s => E__ee g e1 (BSemi es s)
  | E__pat g m e cl                   s => TODO (* disabled while MGoal is disabled *)
  (*
    match next m with
    | None         => E__v g (ExprMGoal cl) s
    | Some (σ, m') => E__vx g (σ e) (Pat g m' e cl s)
    end
   *)
  | E__args g []                      s => A__args s ([] : values)
  | E__args g (e :: es)               s => E__v g e (Args g es s)
  | E__seq [] []                      s => A__x s ([] : goals)
  | E__seq (g :: gs) (e :: es)        s => E__ee g e (Seq1 gs es s)
  | E__vx g e                         s => E__v g e (EExec1 g s)
  | E__ee g e                         s => E__v g e (EExpr g s)

  (** Figure 8.2 *)

  | A__args (Args1 g l s) (Fail__RARGS n) => A__v s n

  | A__args (Args1 g (Lam xs e) s) (Vals__RARGS vs) =>
    match Nat.compare (List.length vs) (List.length xs) with
    | Lt =>
      let lv := List.length vs in
      let xs1 := firstn lv xs in
      let xs2 := firstn lv xs in
      A__v s (Lam xs2 (subst_exprs xs1 (List.map ExprVal vs) e))
    | Eq => E__v g (subst_exprs xs (List.map ExprVal vs) e) s
    | Gt =>
      let lx := List.length xs in
      let vs1 := firstn lx vs in
      let vs2 := firstn lx vs in
      E__v g (subst_exprs xs (List.map ExprVal vs1) e) s
    end

  | A__args (Args2 v s) (Fail__RARGS n) => A__args s n

  | A__args (Args2 v s) (Vals__RARGS vs) => A__args s ((v :: vs) : values)

  (** Figure 8.3 *)

  | A__x (Prog g s) (Fail__RX n)   => A__x s n
  | A__x (Prog g s) (Goals__RX gs) =>
    A__x s (
          match gs with
          | [g'] =>
            if g == g'
            then Fail 0
            else Goals__RX gs
          | _ => Goals__RX gs
          end
        )
  | A__x (First g es s) (Fail 0)     => E__x g (TacFirst es) s
  | A__x (First g es s) (Fail (S n)) => A__x s (Fail n)
  | A__x (First g es s) (Goals__RX gs) => A__x s (Goals__RX gs)
  | A__x (Semi e s) (Fail__RX n)       => A__x s (Fail__RX n)
  | A__x (Semi e s) (Goals__RX gs)     => E__seq gs (List.repeat e (List.length gs)) s
  | A__x (BSemi es s) (Fail__RX n)     => A__x s (Fail__RX n)
  | A__x (BSemi es s) (Goals__RX gs)   =>
    if List.length gs == List.length es
    then E__seq gs es s
    else A__x s (Fail 0)
  | A__x (Seq1 gs es s) (Fail__RX n)    => A__x s (Fail__RX n)
  | A__x (Seq1 gs es s) (Goals__RX gs') => E__seq gs es (Seq2 gs' s)
  | A__x (Seq2 gs s) (Fail__RX n)       => A__x s n
  | A__x (Seq2 gs s) (Goals__RX gs')    => A__x s ((gs ++ gs')%list : goals)
  | A__x (EExec2 s) r => A__v s r

  (** Figure 8.4 *)

  | A__v (Let g x e2 s)   (Goals__RV gs) => A__v s (Fail 0)
  | A__v (Let g x e2 s)   (Fail__RV n)   => A__v s n
  | A__v (Let g x e2 s)   (Value__RV v)  => E__v g (subst_expr x v e2) s
  | A__v (App g es s)     (Fail__RV n)   => A__v s (Fail__RV n)
  | A__v (App g es s)     (ValLam l)   => E__args g es (Args1 g l s)
  | A__v (App g es s)     _            => A__v s (Fail 0)
  | A__v (Args g es s)    (Value__RV v)  => E__args g es (Args2 v s)
  | A__v (Args g es s)    (Goals__RV gs) => A__args s (Fail 0)
  | A__v (Args g es s)    (Fail__RV n)   => A__args s (Fail__RARGS n)
  | A__v (Pat g m e cl s) (Value__RV v)  => A__v s v
  | A__v (Pat g m e cl s) (Goals__RV gs) => A__v s gs
  | A__v (Pat g m e cl s) (Fail 0)     => E__pat g m e cl s
  | A__v (Pat g m e cl s) (Fail (S n)) => A__v s (Fail n)
  | A__v (EExec1 g s)     (Fail__RV n)   => A__v s n
  | A__v (EExec1 g s)     (Goals__RV gs) => A__v s gs
  | A__v (EExec1 g s)     (ValInt n)   => A__v s n
  | A__v (EExec1 g s)     (ValLam l)   => A__v s l
  | A__v (EExec1 g s)     (ValTac t)   => E__x g t (EExec2 s)
  | A__v (EExpr g s)      (Fail__RV n)   => A__x s n
  | A__v (EExpr g s)      (Goals__RV gs) => A__x s gs
  | A__v (EExpr g s)      (ValInt n)   => A__x s (Fail 0)
  | A__v (EExpr g s)      (ValLam l)   => A__x s (Fail 0)
  | A__v (EExpr g s)      (ValTac t)   => E__x g t s

  (* Final configuration steps to itself *)

  | A__x Nil r => A__x Nil r

  (* Missing from the paper: *)
  (*
  | E__v _ (ExprVar _) _ => TODO
   *)

  | E__x g (TacAtom atac) s =>
    match atomic_exec g atac with
    | None    => A__x s (Fail 0)
    | Some gs => A__x s gs
    end

  (* by construction, I believe this never happens: *)
  | E__seq [] (_ :: _) _ => TODO
  | E__seq (_ :: _) [] _ => TODO

  end.

(*
Fixpoint step (n : nat) (c : configuration) : configuration :=
  match n with
  | O => c
  | S n' => step n' (stepConfiguration c)
  end.
 *)

Definition step_one a b := stepConfiguration a = b.

Notation "a ⇒ b" := (step_one a b) (at level 50).

Definition step_star := clos_refl_trans configuration step_one.

Notation "a ⇒* b" := (clos_refl_trans _ step_one a b) (at level 50).

(*
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
 *)

Global Instance Reflexive_clos_refl_trans :
  forall T (R : T -> T -> Prop), Reflexive (clos_refl_trans _ R).
Proof.
  repeat intro.
  now constructor.
Qed.

Global Instance Transitive_clos_refl_trans :
  forall T (R : T -> T -> Prop), Transitive (clos_refl_trans _ R).
Proof.
  repeat intro.
  now econstructor; eauto.
Qed.
