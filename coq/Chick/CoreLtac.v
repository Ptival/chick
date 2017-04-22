From Coq Require Import
     EquivDec
     List
     Omega
     Operators_Properties
     Relation_Operators
     String
.

From Chick Require Import
     Tactic.Atomic
     Goal
     LocalContext
     LocalDeclaration
     ReservedNotations
     TODO
.
Require Import Chick.Variable.

From Chick.Tactic.Atomic.Semantics Require Import
     Agreement
     Functional
     Relational
.

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
| ValLam : forall (l : lam), value
| ValTac : forall (t : tactic), value
| ValInt : forall (n : nat), value

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

Scheme expr_ind'   := Induction for expr   Sort Prop
  with value_ind'  := Induction for value  Sort Prop
  with tactic_ind' := Induction for tactic Sort Prop
.

Combined Scheme ltac_ind from expr_ind', value_ind', tactic_ind'.

Inductive fail := Fail : forall (l : nat), fail.

Inductive RV :=
| Fail__RV  : forall (l : fail), RV
| Goals__RV : forall (gs : goals), RV
| Lam__RV   : forall (l : lam), RV
| Int__RV   : forall (n : nat), RV
| Tac__RV   : forall (t : tactic), RV
.

Coercion Fail__RV  : fail   >-> RV.
Coercion Goals__RV : goals  >-> RV.
Coercion Lam__RV   : lam    >-> RV.
Coercion Int__RV   : nat      >-> RV.
Coercion Tac__RV   : tactic >-> RV.

Definition coerce_value_to_RV (v : value) : RV :=
  match v with
  | ValLam l => Lam__RV l
  | ValInt n => Int__RV n
  | ValTac t => Tac__RV t
  end.

Coercion coerce_value_to_RV : value >-> RV.

Inductive RVX :=
| Fail__RVX  : forall (l : fail), RVX
| Goals__RVX : forall (gs : goals), RVX
| Lam__RVX   : forall (l : lam), RVX
| Int__RVX   : forall (n : nat), RVX
.

Coercion Fail__RVX  : fail  >-> RVX.
Coercion Goals__RVX : goals >-> RVX.
Coercion Lam__RVX   : lam   >-> RVX.
Coercion Int__RVX   : nat     >-> RVX.

Definition coerce_RVX_to_RV (rvx : RVX) : RV :=
  match rvx with
  | Fail__RVX  n  => Fail__RV  n
  | Goals__RVX gs => Goals__RV gs
  | Lam__RVX   l  => Lam__RV   l
  | Int__RVX   n  => Int__RV   n
  end.

Coercion coerce_RVX_to_RV : RVX >-> RV.

Inductive RX :=
| Fail__RX  : forall (l : fail), RX
| Goals__RX : forall (gs : goals), RX
.

Coercion Fail__RX  : fail  >-> RX.
Coercion Goals__RX : goals >-> RX.

Definition coerce_RX_to_RVX (rx : RX) : RVX :=
  match rx with
  | Fail__RX  n  => Fail__RVX  n
  | Goals__RX gs => Goals__RVX gs
  end.

Coercion coerce_RX_to_RVX : RX >-> RVX.

Definition values := list value.

Inductive RARGS :=
| Fail__RARGS : forall (l : fail), RARGS
| Vals__RARGS : forall (vs : values), RARGS
.

Coercion Fail__RARGS  : fail   >-> RARGS.
Coercion Vals__RARGS  : values >-> RARGS.

Parameter subst_expr : variable -> expr -> expr -> expr.

Parameter subst_exprs : list variable -> list expr -> expr -> expr.

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

| VAL : forall v r,
    r = match v return RV with
        | ValLam l => l
        | ValTac t => t
        | ValInt n => n
        end ->
    g ⊢ ExprVal v ↓v r

| FIX : forall x e rv,
    g ⊢ subst_expr x (ExprFix x e) e ↓v rv ->
    g ⊢ ExprFix x e                  ↓v rv

| APP1 : forall e es (l : fail),
    g ⊢ e            ↓v l ->
    g ⊢ ExprApp e es ↓v l

| APP2_Int : forall e es (n : nat),
    g ⊢ e            ↓v n      ->
    g ⊢ ExprApp e es ↓v Fail 0

| APP2_Tac : forall e es (t : tactic),
    g ⊢ e            ↓v t      ->
    g ⊢ ExprApp e es ↓v Fail 0

| APP3 : forall e es (l : lam) (n : fail),
    g ⊢ e            ↓v    l ->
    g ⊢ es           ↓args n ->
    g ⊢ ExprApp e es ↓v    n

| APP4 : forall e es xs e' (vs : values) xs1 xs2,
    List.length vs < List.length xs       ->
    xs1 = List.firstn (List.length vs) xs ->
    xs2 = List.skipn  (List.length vs) xs ->
    g ⊢ e            ↓v    Lam xs e'      ->
    g ⊢ es           ↓args vs             ->
    g ⊢ ExprApp e es ↓v    Lam__RV (Lam xs2 (subst_exprs xs1 (List.map ExprVal vs) e))

where

"g ⊢ e ↓v r" := (ExprEval g e r)

.

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
      g ⊢ e ↓v Fail n ->
      g ⊢ e ⇓  Fail n

| EEX4_1 :
    forall g e (n : nat),
      g ⊢ e ↓v n      ->
      g ⊢ e ⇓  Fail 0

| EEX4_2 :
    forall g e (l : lam),
      g ⊢ e ↓v l      ->
      g ⊢ e ⇓  Fail 0

where

"g ⊢ e ⇓ r" := (ExprExec g e r)

with

TacExec : goal -> tactic -> RX -> Prop :=

| ATOMIC_FAIL :
    forall g a,
      g ⊢ a ⇓a None           ->
      g ⊢ TacAtom a ↓x Fail 0

| ATOMIC_SUCCESS :
    forall g a gs,
      g ⊢ a ⇓a Some gs    ->
      g ⊢ TacAtom a ↓x gs

| IDTAC :
    forall g,
      g ⊢ TacIdtac ↓x Goals__RX [g]

| FAIL :
    forall g n,
      g ⊢ TacFail n ↓x Fail n

| FIRST1 :
    forall g,
      g ⊢ TacFirst [] ↓x Fail 0

| FIRST2 :
    forall g e es (gs : goals),
      g ⊢ e                  ⇓  gs ->
      g ⊢ TacFirst (e :: es) ↓x gs

| FIRST3 :
    forall g e es n,
      g ⊢ e                  ⇓  Fail (S n) ->
      g ⊢ TacFirst (e :: es) ↓x Fail n

| FIRST4 :
    forall g e es rx,
      g ⊢ e                  ⇓  Fail 0 ->
      g ⊢ TacFirst es        ↓x rx     ->
      g ⊢ TacFirst (e :: es) ↓x rx

| PROGR1 :
    forall g e n,
      g ⊢ e             ⇓  Fail n ->
      g ⊢ TacProgress e ↓x Fail n

| PROGR2 :
    forall g e,
      g ⊢ e             ⇓  ([g] : goals) ->
      g ⊢ TacProgress e ↓x Fail 0

| PROGR3 :
    forall g e (gs : goals),
      g ⊢ e             ⇓  gs ->
      gs <> [g]                ->
      g ⊢ TacProgress e ↓x gs

| SEMI1 :
    forall g e1 e2 (f : fail),
      g ⊢ e1            ⇓  f ->
      g ⊢ TacSemi e1 e2 ↓x f

| SEMI2 :
    forall g e1 e2 (gs : goals) rx,
      g ⊢ e1            ⇓  gs ->
      gs ⊢ List.repeat e2 (List.length gs) ↓seq rx ->
      g ⊢ TacSemi e1 e2 ↓x rx

| BRANCH1 :
    forall g e1 es (f : fail),
      g ⊢ e1                  ⇓  f ->
      g ⊢ TacBranch e1 es ↓x f

| BRANCH2 :
    forall g e1 es (gs : goals),
      g ⊢ e1                  ⇓  gs   ->
      List.length gs <> List.length es ->
      g ⊢ TacBranch e1 es ↓x Fail 0

| BRANCH3 :
    forall g e1 es (gs : goals) rx,
      g ⊢ e1                  ⇓    gs     ->
      List.length gs = List.length es     ->
      gs ⊢ es                 ↓seq rx     ->
      g ⊢ TacBranch e1 es ↓x   rx

where

"g ⊢ e ↓x r" := (TacExec g e r)

with TacSeqExec : goals -> list expr -> RX -> Prop :=

| SEQ1 :
    [] ⊢ [] ↓seq ([] : goals)

| SEQ2 :
    forall g e gs es (f : fail),
      g       ⊢ e       ⇓    f ->
      g :: gs ⊢ e :: es ↓seq f

| SEQ3 :
    forall g e gs es (gs' : goals) (f : fail),
      g       ⊢ e       ⇓    gs' ->
      gs      ⊢ es      ↓seq f   ->
      g :: gs ⊢ e :: es ↓seq f

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
    forall (n : fail),
      g ⊢ e ↓v  n ->
      g ⊢ e ↓vx n
| EEV4_1 :
    forall (n : nat),
      g ⊢ e ↓v  n ->
      g ⊢ e ↓vx n
| EEV4_2 :
    forall (l : lam),
      g ⊢ e ↓v  l ->
      g ⊢ e ↓vx l
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
| Args1 : forall (g : goal) (l : lam) (s : S_v), S_a
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
       | Args1 _ _ s => S (S_v_size s)
       | Args2 _   s => S (S_a_size s)
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
| E__v    : forall (g : goal) (e : expr) (s : S_v), configuration
    (* I think there's a typo in the thesis here, is says A__v takes a RX *)
| A__v    : forall (s : S_v) (r : RV), configuration
| E__x    : forall (g : goal) (t : tactic) (s : S_x), configuration
| A__x    : forall (s : S_x) (r : RX), configuration
| E__ee   : forall (g : goal) (e : expr) (s : S_x), configuration
| E__vx   : forall (g : goal) (e : expr) (s : S_v), configuration
| E__pat  : forall (g : goal) (m : matcher) (e : expr) (cl : list (pattern * expr))
                 (s : S_v), configuration
| E__args : forall (g : goal) (es : list expr) (s : S_a), configuration
| A__args : forall (s : S_a) (r : RARGS), configuration
| E__seq  : forall (g : list goal) (es : list expr) (s : S_x), configuration
.

Definition initialconfiguration (g : goal) (e : expr) :=
  E__ee g e Nil.

Parameter start : goal -> pattern -> matcher.
Parameter next : matcher -> option ((expr -> expr) * matcher).

Definition stepConfiguration (c : configuration) : configuration :=
  match c with

  (** Figure 8.1 *)

  | E__v g (ExprVal (ValLam l))       s => A__v s l
  | E__v g (ExprVal (ValTac t))       s => A__v s t
  | E__v g (ExprVal (ValInt n))       s => A__v s n

  | E__v g (ExprApp e es)             s => E__v g e (App g es s)

  | E__v g (ExprLet x e1 e2)          s => E__v g e1 (Let g x e2 s)

  | E__v g (ExprMGoal [])             s => A__v s (Fail 0)
  | E__v g (ExprMGoal ((p, e) :: cl)) s => E__pat g (start g p) e cl s

  | E__v g (ExprFix x e)              s => E__v g (subst_expr x (ExprFix x e) e) s

  | E__x g TacIdtac                   s => A__x s ([g] : goals)

  | E__x g (TacFail n)                s => A__x s (Fail n)

  | E__x g (TacProgress e)            s => E__ee g e (Prog g s)

  | E__x g (TacFirst [])              s => A__x s (Fail 0)

  | E__x g (TacFirst (e :: es))       s => E__ee g e (First g es s)

  | E__x g (TacSemi e1 e2)            s => E__ee g e1 (Semi e2 s)

  | E__x g (TacBranch e1 es)      s => E__ee g e1 (BSemi es s)

  | E__pat g m e cl                   s =>
    match next m with
    | None         => E__v g (ExprMGoal cl) s
    | Some (σ, m') => E__vx g (σ e) (Pat g m' e cl s)
    end

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
      A__v s (Lam__RV (Lam xs2 (subst_exprs xs1 (List.map ExprVal vs) e)))
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

  | A__v (Let g x e2 s) (Goals__RV gs) => A__v s (Fail 0)

  | A__v (Let g x e2 s) (Fail__RV n)   => A__v s n

  | A__v (Let g x e2 s) (Lam__RV l)    => E__v g (subst_expr x (ExprVal l) e2) s

  | A__v (Let g x e2 s) (Int__RV n)    => E__v g (subst_expr x (ExprVal (ValInt n)) e2) s

  | A__v (Let g x e2 s) (Tac__RV t)    => E__v g (subst_expr x (ExprVal (ValTac t)) e2) s

  | A__v (App g es s) (Fail__RV n)   => A__v s (Fail__RV n)

  | A__v (App g es s) (Lam__RV l) => E__args g es (Args1 g l s)

  | A__v (App g es s) _            => A__v s (Fail 0)

  | A__v (Args g es s) (Lam__RV l) => E__args g es (Args2 l s)
  | A__v (Args g es s) (Int__RV n) => E__args g es (Args2 n s)
  | A__v (Args g es s) (Tac__RV t) => E__args g es (Args2 t s)

  | A__v (Args g es s) (Goals__RV gs) => A__args s (Fail 0)

  | A__v (Args g es s) (Fail__RV n)   => A__args s (Fail__RARGS n)

  | A__v (Pat g m e cl s) (Lam__RV l) => A__v s l
  | A__v (Pat g m e cl s) (Int__RV n) => A__v s n
  | A__v (Pat g m e cl s) (Tac__RV t) => A__v s t

  | A__v (Pat g m e cl s) (Goals__RV gs)   => A__v s gs

  | A__v (Pat g m e cl s) (Fail 0)     => E__pat g m e cl s

  | A__v (Pat g m e cl s) (Fail (S n)) => A__v s (Fail n)

  | A__v (EExec1 g s) (Fail__RV n)   => A__v s n

  | A__v (EExec1 g s) (Goals__RV gs) => A__v s gs

  | A__v (EExec1 g s) (Int__RV n)    => A__v s n

  | A__v (EExec1 g s) (Lam__RV l) => A__v s l

  | A__v (EExec1 g s) (Tac__RV t)    => E__x g t (EExec2 s)

  | A__v (EExpr g s) (Fail__RV n)   => A__x s n

  | A__v (EExpr g s) (Goals__RV gs) => A__x s gs

  | A__v (EExpr g s) (Int__RV n)    => A__x s (Fail 0)

  | A__v (EExpr g s) (Lam__RV l)    => A__x s (Fail 0)

  | A__v (EExpr g s) (Tac__RV t)    => E__x g t s

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

(*
Theorem decompose_star :
  forall n cStart cMiddle cFinal,
    step n cStart = cMiddle ->
    cMiddle ⇒* cFinal ->
    cStart ⇒* cFinal.
Proof.
  intros.
  etransitivity; [ | eauto ].
  eexists (_ + _).
  eapply decompose_step; eauto.
Qed.

Ltac step n := eapply (decompose_star n); [ reflexivity | simpl ].
 *)

Local Ltac step := etransitivity; [ apply rt_step; reflexivity | simpl ].

Tactic Notation "on_context" open_constr(c) tactic(tac) :=
  match goal with [ H : context [c] |- _ ] => tac H end.

Theorem abstractMachineCorrect_val :
  forall (g : goal) (e : expr) (r : RV),
    g ⊢ e ↓v r -> (forall s, E__v g e s ⇒* A__v s r).
Proof.
  do 4 intro.
  on_head ExprEval induction';  intros;
    try solve
        [ now step
        | step; find_rewrite_r; now step
        | step; break_match_in_goal; now subst
        ].
Qed.

Theorem abstractMachineCorrect_exec :
  (forall g  e  r, g  ⊢ e  ⇓    r -> forall s, E__ee  g  e  s ⇒* A__x s r)
  /\
  (forall g  t  r, g  ⊢ t  ↓x   r -> forall s, E__x   g  t  s ⇒* A__x s r)
  /\
  (forall gs es r, gs ⊢ es ↓seq r -> forall s, E__seq gs es s ⇒* A__x s r)
.
Proof.
  apply (
      Exec_ind
        (fun g  e  r => forall s, E__ee  g  e  s ⇒* A__x s r)
        (fun g  t  r => forall s, E__x   g  t  s ⇒* A__x s r)
        (fun gs es r => forall s, E__seq gs es s ⇒* A__x s r)
    );
    intros;
    try solve
        [ now step
        | step; find_rewrite_r; now step
        | step;
          on ExprEval ltac:(in_eapply abstractMachineCorrect_val);
          on_head clos_refl_trans rewrite_r;
          now step
        | step; find_apply_in_hyp agreement; find_rewrite_r; now constructor
        | step; find_rewrite_r; step; find_rewrite_r; now step
        ].
  - step.
    find_rewrite_r.
    step.
    break_if_in_goal.
    + reflexivity.
    + congruence.
  - step.
    find_rewrite_r.
    step.
    break_match_in_goal; subst_all.
    + reflexivity.
    + break_match_in_goal; subst_all.
      * break_if_in_goal; subst_all.
        { congruence. }
        { reflexivity. }
      * reflexivity.
  - step.
    find_rewrite_r.
    step.
    break_if_in_goal; subst_all.
    + congruence.
    + reflexivity.
  - step.
    find_rewrite_r.
    step.
    break_if_in_goal; subst_all.
    + now find_rewrite_r.
    + congruence.
Qed.

Local Ltac next :=
  on clos_refl_trans inversion'; [ try solve [ on step_one inversion' ] | ].

Theorem next_configuration : forall (P : Prop) c c',
    c <> c' ->
    (stepConfiguration c ⇒* c' -> P) ->
    c ⇒* c' -> P.
Proof.
  intros.
  on_head clos_refl_trans
          ltac:(
    in_eapply (
        clos_refl_trans_ind_right
          _ step_one (fun cInd => cInd = c -> P)
  )).
  - assumption.
  - congruence.
  - intros. subst. on_head step_one inversion'. auto.
  - reflexivity.
Qed.

Ltac forward :=
  on_head clos_refl_trans
          ltac:(fun H =>
                  eapply next_configuration in H;
                  [ eassumption
                  | discriminate
                  | simpl; clear H; intro
                  ]
               ).

Lemma A__x_Nil_steps_to_itself : forall c c',
    c ⇒* c' ->
    forall r,
    c = A__x Nil r ->
    c' = A__x Nil r.
Proof.
  do 4 intro.
  on clos_refl_trans induction'; intros; subst.
  - now on step_one inversion'.
  - reflexivity.
  - auto.
Qed.

Local Ltac step_A__x :=
  on_context (_ ⇒* _) ltac:(fun H => eapply A__x_Nil_steps_to_itself in H; eauto).

Notation "Γ ⊢ a ⇒ v" := (atomic_exec Γ a = Some v).

Ltac done :=
  find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto; subst_all.

Lemma abstractMachineCorrect :
  (forall (e : expr)   g r, E__ee g e Nil ⇒* A__x Nil r -> g ⊢ e ⇓  r)
  /\
  (forall (v : value)  g r, E__ee g v Nil ⇒* A__x Nil r -> g ⊢ v ↓v r)
  /\
  (forall (t : tactic) g r, E__ee g t Nil ⇒* A__x Nil r -> g ⊢ t ↓x r).
Proof.
  eapply (
      ltac_ind
        (fun e => forall g r, E__ee g e Nil ⇒* A__x Nil r -> g ⊢ e ⇓  r)
        (fun v => forall g r, E__ee g v Nil ⇒* A__x Nil r -> g ⊢ v ↓v r)
        (fun t => forall g r, E__ee g t Nil ⇒* A__x Nil r -> g ⊢ t ↓x r)
    ); intros.
  {
    admit.
  }
  {
    admit.
  }
  {
    forward.
    forward.
    forward.
    break_match_in_hyp.
    { break_match_in_hyp.
      { forward.
        forward.
        break_match_in_hyp.
        { break_match_in_hyp.
          { forward.
            done.
            eapply EEX4_2.
            eapply ExprLet.
            now repeat econstructor.

            forward.
            forward.
            forward.
            forward.
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }
  {
    admit.
  }

Lemma abstractMachineCorrect_tac:
  forall g (t : tactic) r,
    E__ee g t Nil ⇒* A__x Nil r -> g ⊢ t ↓x r.
Proof.
  intros.
  on_head tactic induction'.

  { (* g ⊢ atac ⇓ r *)
    repeat progress forward.
    break_match_in_hyp.
    { step_A__x.
      subst_all.
      find_apply_in_hyp agreement.
      now econstructor.
    }
    { find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto.
      subst_all.
      find_eapply_in_hyp agreement.
      now constructor.
    }
  }

  { (* g ⊢ idtac ⇓ r *)
    repeat forward.
    find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto.
    subst_all.
    now constructor.
  }

  { (* g ⊢ fail n ⇓ r *)
    repeat forward.
    find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto.
    subst_all.
    now constructor.
  }

  { (* g ⊢ e1 ; e2 ⇓ r *)
    repeat forward.
    break_match_in_hyp; subst_all.

    { (* g ⊢ v ; e2 ⇓ r *)
      break_match_in_hyp; subst_all.

      { (* g ⊢ λ xs . e ; e2 ⇓ r *)
        repeat forward.
        find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto.
        subst_all.
        apply SEMI1.
        eapply EEX4_2.
        now apply VAL.
      }

      { (* g ⊢ t ; e2 ⇓ r *)
        forward.
        forward.
        break_match_in_hyp; subst_all.
        { break_match_in_hyp; subst_all.
          { generalize dependent g0. revert r a.
            on_head expr induction'; intros.
            admit.
            { repeat forward.
              break_match_in_hyp.
              simpl in *.
              find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto.
              subst_all.
              eapply SEMI2.
              eapply EEX2; [ now apply VAL | ].
              apply ATOMIC_SUCCESS.
              apply agreement.
              eassumption.
              constructor.
              simpl in H.
              subst_all.
              forward.
              forward.
              forward.
              forward.
              forward.
              forward.
              forward.
              forward.
              eapply EEX1. eapply agreement. apply VAL.
              admit.[
            }
            { repeat forward.
              break_match_in_hyp; subst_all; simpl in *.
              { find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto.
                subst_all.
                eapply SEMI2.
                { eapply EEX2; [ now apply VAL | ].
                  apply ATOMIC_SUCCESS.
                  apply agreement.
                  eassumption.
                }
                {
                  constructor.
                  constructor.
                }
              }
              { repeat forward.
                break_match_in_hyp; subst_all.
                { break_match_in_hyp; subst_all.
                  { forward.
                    (*
H : A__x (Seq1 g2 (repeat l (Datatypes.length g2)) Nil) (Fail 0) ⇒* A__x Nil r
                   *)
                  repeat forward.
                  find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto.
                  subst_all.
                  eapply SEMI2.
                  { eapply EEX2; [ now apply VAL | ].
                    apply ATOMIC_SUCCESS.
                    apply agreement.
                    eassumption.
                  }
                  { constructor.
                    eapply EEX4_2.
                    now apply VAL.
                  }
                }
                { repeat forward.
                  break_match_in_hyp; subst_all.
                  { break_match_in_hyp; subst_all.
                    {
                  (*
H : A__x (Seq1 g2 (repeat l (Datatypes.length g2)) Nil) (Fail 0) ⇒* A__x Nil r
                   *)
                      repeat forward.
                      find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto.
                      subst_all.
                      eapply SEMI2.
                      { eapply EEX2; [ now apply VAL | ].
                        apply ATOMIC_SUCCESS.
                        apply agreement.
                        eassumption.
                      }
                      { constructor.
                        eapply EEX4_2.
                        now apply VAL.
                      }
                    }
                    { repeat forward.
                      break_match_in_hyp; subst_all.
                      { break_match_in_hyp; subst_all.
                        { repeat forward.
                          find_eapply_in_hyp A__x_Nil_steps_to_itself; eauto.
                          subst_all.
                          eapply SEMI2.
                          { eapply EEX2; [ now apply VAL | ].

Theorem abstractMachineCorrect_expr' :
  forall g e r, E__ee g e Nil ⇒* A__x Nil r -> g ⊢ e ⇓ r.
Proof.
  intros.
  on expr destruct'.

  { (* g ⊢ v ⇓ r *)
    on value destruct'.

    { (* g ⊢ λ xs . e ⇓ r *)
      on RX destruct'.

      { (* g ⊢ λ xs . e ⇓ ⊥ *)
        on_head fail destruct'.
        on_head nat destruct'.
        { eapply EEX4_2.
          now constructor.
        }
        { repeat progress forward.
          now step_A__x.
        }
      }
      { repeat progress forward.
        now step_A__x.
      }
    }

    { (* g ⊢ t ⇓ r *)
      eapply EEX2; [ now apply VAL | ].
      on_head tactic destruct'.


            eauto.
            repeat constructor.










            eapply (
              clos_refl_trans_ind_right
                _ step_one
                (fun c : configuration => c = E__ee g (ExprVal (ValLam xs e)) Nil -> False)
    ) in H.
  exact H.
  congruence.


  ; try congruence; intros; subst.

  inversion' H0.
  simpl in *; subst. clear H1.
  inversion H0.


  (*
  pose proof (
         clos_refl_trans_ind_left
           configuration step_one c
           (fun c1 : configuration => c1 = A__x Nil (Fail__RX n) -> False)
       ).
  eapply H0 in H. exact H. congruence.
  apply H. subst. simpl in *.
  specialize (
      H0 _ _ _ c0 H
    ).
    *)

  pose proof (
         clos_refl_trans_ind_right
           configuration step_one
           (fun c : configuration => c = E__ee g (ExprVal (ValLam xs e)) Nil -> False) c0
       ) as IND.
  eapply IND in H; eauto. congruence.
  intros. subst.




  inversion H0.
          next.
          inversion H.


          apply EEX3.
          apply VAL.
          constructor.
          inversion H. inversion H0. simpl in H0.
          next.



  match goal with
  | [_ : ?a ⇒* ?b |- _] => remember a; remember b
  end.
  induction H. admit. admit.
  on clos_refl_trans ltac:(fun H => induction H using clos_refl_trans_ind_left); intros.
  admit.
  auto.
Qed.

Theorem abstractMachineCorrect_exec' :
  forall g,
    (forall e r, E__ee g e Nil ⇒* A__x Nil r -> g ⊢ e ⇓ r)
    /\
    (forall t r, E__x g t Nil ⇒* A__x Nil r -> g ⊢ t ↓x r)
.
Proof.
  split; intros.
  {
    match goal with
    | [_ : ?a ⇒* ?b |- _] => remember a; remember b
    end.
    repeat on @eq revert'.
    induction H; intros; subst.
    inversion H.
    inversion Heqc0.
    subst.
    admit.

    on clos_refl_trans ltac:(fun H => induction H using clos_refl_trans_ind_left); intros.
    { congruence. }
    subst.
    on ExprExec ltac:(fun H => specialize (H eq_refl)).
    on_head step_one inversion'.
    on_head configuration destruct'; on @eq simpl'.
    - break_match_in_hyp; subst; try congruence.
      + break_match_in_hyp; subst; congruence.
      + break_match_in_hyp; subst.
        * congruence.
        * break_let_pair_in_hyp; congruence.
    - break_match_in_hyp; subst.
      + break_match_in_hyp; subst; try congruence.
      + break_match_in_hyp; subst; try congruence.
      + break_match_in_hyp; subst; try congruence.
      + break_match_in_hyp; subst; try congruence.
        * break_match_in_hyp; subst; try congruence.
      + break_match_in_hyp; subst; try congruence.
      + break_match_in_hyp; subst; try congruence.
        * on @eq inversion'.
          on clos_refl_trans inversion'.
          { on step_one inversion'. }

Qed.

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
  | ECApp (ECExpr g _) (LCFail n) => Fail__RV n

  (* prop_app *)
  | ECExpr g (ExprApp e es) => ECApp (ECExpr g e) (LCExprs g es)

  (* app_l_bot *)
  | ECApp (Fail__RV n) cs => Fail__RV n

  (* app_l_nval *)
  | ECApp _ cs => Fail__RV 0

  (* prop_let *)
  | ECExpr g (ExprLet x e1 e2) => ECLet x (ECExpr g e1) (ECExpr g e2)

  (* let_gs *)
  | ECLet x (Goals__RV gs) c => Fail__RV 0

  (* let_bot *)
  | ECLet x (Fail__RV n) c => Fail__RV n

  (* let_v *)
  | ECLet x v c => subst_exprclosure x v c

  (* mg_nil *)
  | ECExpr g (ExprMGoal []) => Fail__RV 0

  (* prop_mg *)
  | ECExpr g (ExprMGoal ((p, e) :: cl)) => ECMGoal (start g p) (ECExpr g e) cl

  (* mg_none *)
  | ECMGoal m (ECExpr g e) cl =>
    match next m with
    | None         => ECExpr g (ExprMGoal cl)
    | Some (σ, m') => ECMEval m' (g, e, ECExec1 g (ECExpr g (σ e))) cl
    end

  (* mg_gs *)
  | ECMEval m' (g, e, Goals__RV gs) cl => Goals__RV gs

  (* mg_bot0 *)
  | ECMEval m' (g, e, Fail__RV 0) cl => ECMGoal m' (ECExpr g e) cl

  (* mg_botS *)
  | ECMEval m' (g, e, Fail__RV (S n)) cl => Fail__RV n

  (* mg_val *)
  | ECMEval m' (g, e, v) cl => v

  (* fix *)
  | ECExpr g (ExprFix x e) => ECExpr g (subst_expr x (ExprFix x e) e)

  (* exec1_tac *)
  | ECExec1 g (Tac__RV t) => ECEval (TCTac g t)

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
  | TCTac g TacIdtac => Goals__RX [g]

  (* fail *)
  | TCTac g (TacFail n) => Fail__RX n

  (* prop_progress *)
  | TCTac g (TacProgress e) => TCProgress g (TCExec g e)

  (* progress_eq + progress_neq *)
  | TCProgress g (Goals__RX gs) =>
    if list_eq_dec goal_eq_dec [g] gs
    then Fail__RX 0
    else Goals__RX gs

  (* progress_bot *)
  | TCProgress g (Fail__RX n) => Fail__RX n

  (* semi_bot_l *)
  | TCSemi (Fail__RX n) _ => Fail__RX n

  (* semi_gs *)
  | TCSemi (Goals__RX gs) e => TCSeq (SeqExprs gs (repeat e (length gs)))

  (* bsemi_bot_l *)
  | TCBSemi (Fail__RX n) _ => Fail__RX n

  (* bsemi_gs *)
  | TCBSemi (Goals__RX gs) es =>
    if Nat.eq_dec (length gs) (length es)
    then Fail__RX 0
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
