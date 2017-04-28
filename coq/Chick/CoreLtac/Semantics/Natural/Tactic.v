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
     Semantics.Natural.Atomic
     Syntax
.

From HaysTac Require Import HaysTac.

Import ListNotations.

Inductive ArgsEval (g : goal) : list expr -> RARGS -> Type :=

where

"g ⊢ es ↓args r" := (ArgsEval g es r)

.

Inductive ExprEval (g : goal) : expr -> RV -> Prop :=

(** Missing from the paper *)

(*
| VAR : forall v r,
    ??? ->
    ExprEval g (ExprVar v) r
 *)

(** Figure 7.4 *)

| VAL : forall v,
    g ⊢ ExprVal v ↓v v

| FIX : forall x e rv,
    g ⊢ subst_expr x (ExprFix x e) e ↓v rv ->
    g ⊢ ExprFix x e                  ↓v rv

| APP1 : forall e es (l : fail),
    g ⊢ e            ↓v l ->
    g ⊢ ExprApp e es ↓v l

| APP2__INT : forall e es n,
    g ⊢ e            ↓v ⎡n⎤ ->
    g ⊢ ExprApp e es ↓v ⊥ 0

| APP2__TAC : forall e es (t : tactic),
    g ⊢ e            ↓v t   ->
    g ⊢ ExprApp e es ↓v ⊥ 0

| APP3 : forall e es (l : lam) n,
    g ⊢ e            ↓v    l ->
    g ⊢ es           ↓args ⊥ n ->
    g ⊢ ExprApp e es ↓v    ⊥ n

| APP4 : forall e es xs e' (vs : values) xs1 xs2,
    List.length vs < List.length xs       ->
    xs1 = List.firstn (List.length vs) xs ->
    xs2 = List.skipn  (List.length vs) xs ->
    g ⊢ e            ↓v    Lam xs e'      ->
    g ⊢ es           ↓args vs             ->
    g ⊢ ExprApp e es ↓v    Lam xs2 (subst_exprs xs1 (List.map ExprVal vs) e)

| APP5 : forall e es xs e' (vs : values) rv,
    List.length vs = List.length xs         ->
    g ⊢ e                   ↓v    Lam xs e' ->
    g ⊢ es                  ↓args vs        ->
    g ⊢ subst_exprs xs es e ↓v    rv        ->
    g ⊢ ExprApp e es        ↓v    rv

| APP6 : forall e es xs e' (vs : values) vs1 vs2 rv,
    List.length xs < List.length vs       ->
    vs1 = List.map ExprVal (List.firstn (List.length xs) vs) ->
    vs2 = List.map ExprVal (List.skipn  (List.length xs) vs) ->
    g ⊢ e                                  ↓v    Lam xs e' ->
    g ⊢ es                                 ↓args vs        ->
    g ⊢ ExprApp (subst_exprs xs vs1 e) vs2 ↓v    rv        ->
    g ⊢ ExprApp e es                       ↓v    rv

| LET1 : forall x e1 e2 (f : fail),
    g ⊢ e1              ↓v f ->
    g ⊢ ExprLet x e1 e2 ↓v f

| LET2 : forall x e1 e2 (gs : goals),
    g ⊢ e1              ↓v gs ->
    g ⊢ ExprLet x e1 e2 ↓v Fail 0

| LET3 : forall x e1 e2 rv (v : value),
    g ⊢ e1                ↓v v  ->
    g ⊢ subst_expr x v e2 ↓v rv ->
    g ⊢ ExprLet x e1 e2   ↓v rv

where

"g ⊢ e ↓v r" := (ExprEval g e r)

.

Hint Resolve FIX VAL.

Inductive ExprExec : goal -> expr -> RX -> Prop :=

| EEX1 :
    forall g e (gs : goals),
      g ⊢ e ↓v gs ->
      g ⊢ e ⇓  gs

| EEX2 :
    forall g e (t : tactic) rx,
      g ⊢ e ↓v t  ->
      g ⊢ t ↓x rx ->
      g ⊢ e ⇓  rx

| EEX3 :
    forall g e n,
      g ⊢ e ↓v ⊥ n ->
      g ⊢ e ⇓  ⊥ n

| EEX4__INT :
    forall g e (n : nat),
      g ⊢ e ↓v ⎡n⎤ ->
      g ⊢ e ⇓  ⊥ 0

| EEX4__LAM :
    forall g e (l : lam),
      g ⊢ e ↓v l      ->
      g ⊢ e ⇓  ⊥ 0

where

"g ⊢ e ⇓ r" := (ExprExec g e r)

with

TacExec : goal -> tactic -> RX -> Prop :=

| ATOMIC_FAIL :
    forall g a,
      g ⊢ a         ⇓a None ->
      g ⊢ TacAtom a ↓x ⊥ 0

| ATOMIC_SUCCESS :
    forall g a gs,
      g ⊢ a         ⇓a Some gs ->
      g ⊢ TacAtom a ↓x gs

| IDTAC :
    forall g,
      g ⊢ TacIdtac ↓x ([g] : goals)

| FAIL :
    forall g n,
      g ⊢ TacFail n ↓x ⊥ n

| FIRST1 :
    forall g,
      g ⊢ TacFirst [] ↓x ⊥ 0

| FIRST2 :
    forall g e es (gs : goals),
      g ⊢ e                  ⇓  gs ->
      g ⊢ TacFirst (e :: es) ↓x gs

| FIRST3 :
    forall g e es n,
      g ⊢ e                  ⇓  ⊥ (S n) ->
      g ⊢ TacFirst (e :: es) ↓x ⊥ n

| FIRST4 :
    forall g e es rx,
      g ⊢ e                  ⇓  ⊥ 0 ->
      g ⊢ TacFirst es        ↓x rx  ->
      g ⊢ TacFirst (e :: es) ↓x rx

| PROGR1 :
    forall g e n,
      g ⊢ e             ⇓  ⊥ n ->
      g ⊢ TacProgress e ↓x ⊥ n

| PROGR2 :
    forall g e,
      g ⊢ e             ⇓  ([g] : goals) ->
      g ⊢ TacProgress e ↓x ⊥ 0

| PROGR3 :
    forall g e (gs : goals),
      g ⊢ e             ⇓  gs ->
      gs <> [g]                ->
      g ⊢ TacProgress e ↓x gs

| SEMI1 :
    forall g e1 e2 n,
      g ⊢ e1            ⇓  ⊥ n ->
      g ⊢ TacSemi e1 e2 ↓x ⊥ n

| SEMI2 :
    forall g e1 e2 (gs : goals) rx,
      g  ⊢ e1                              ⇓    gs ->
      gs ⊢ List.repeat e2 (List.length gs) ↓seq rx ->
      g  ⊢ TacSemi e1 e2                   ↓x   rx

| BRANCH1 :
    forall g e1 es n,
      g ⊢ e1              ⇓  ⊥ n ->
      g ⊢ TacBranch e1 es ↓x ⊥ n

| BRANCH2 :
    forall g e1 es (gs : goals),
      List.length gs <> List.length es ->
      g ⊢ e1              ⇓  gs       ->
      g ⊢ TacBranch e1 es ↓x Fail 0

| BRANCH3 :
    forall g e1 es (gs : goals) rx,
      List.length gs = List.length es ->
      g  ⊢ e1              ⇓    gs    ->
      gs ⊢ es              ↓seq rx    ->
      g  ⊢ TacBranch e1 es ↓x   rx

where

"g ⊢ e ↓x r" := (TacExec g e r)

with TacSeqExec : goals -> list expr -> RX -> Prop :=

| SEQ1 :
    [] ⊢ [] ↓seq ([] : goals)

| SEQ2 :
    forall g e gs es n,
      g       ⊢ e       ⇓    ⊥ n ->
      g :: gs ⊢ e :: es ↓seq ⊥ n

| SEQ3 :
    forall g e gs es (gs' : goals) n,
      g       ⊢ e       ⇓    gs' ->
      gs      ⊢ es      ↓seq ⊥ n ->
      g :: gs ⊢ e :: es ↓seq ⊥ n

| SEQ4 :
    forall g e gs es (gs' gs'' : goals),
      g       ⊢ e       ⇓    gs'  ->
      gs      ⊢ es      ↓seq gs'' ->
      g :: gs ⊢ e :: es ↓seq ((gs' ++ gs'')%list : goals)

where

"g ⊢ e ↓seq r" := (TacSeqExec g e r)

.

Scheme ExprExec_ind'   := Minimality for ExprExec   Sort Prop
  with TacExec_ind'    := Minimality for TacExec    Sort Prop
  with TacSeqExec_ind' := Minimality for TacSeqExec Sort Prop
.

Combined Scheme Exec_ind from ExprExec_ind', TacExec_ind', TacSeqExec_ind'.

Inductive ExtendedExprEval (g : goal) (e : expr) : RVX -> Prop :=

| EEV1 :
    forall (gs : goals),
      g ⊢ e ↓v  gs ->
      g ⊢ e ↓vx gs

| EEV2 :
    forall (t : tactic) rx,
      g ⊢ e ↓v  t  ->
      g ⊢ t ↓x  rx ->
      g ⊢ e ↓vx rx

| EEV3 :
    forall n,
      g ⊢ e ↓v  ⊥ n ->
      g ⊢ e ↓vx ⊥ n

| EEV4__INT :
    forall n,
      g ⊢ e ↓v  ⎡n⎤ ->
      g ⊢ e ↓vx ⎡n⎤

| EEV4__LAM :
    forall (l : lam),
      g ⊢ e ↓v  l ->
      g ⊢ e ↓vx l

where

"g ⊢ e ↓vx r" := (ExtendedExprEval g e r)

.

Create HintDb NaturalSemantics.

Hint Constructors ExprEval ExprExec TacExec TacSeqExec : NaturalSemantics.
Hint Extern 0 (_ ⊢ _ ↓v _) => eapply VAL : NaturalSemantics.
Hint Extern 0 (_ ⊢ _ ↓x _) => eapply ATOMIC_SUCCESS : NaturalSemantics.
