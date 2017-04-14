Set Implicit Arguments.
Unset Strict Implicit.

Require Import String.

Definition variable := string.
Definition binder := option variable.

Inductive term :=
| _annot : forall (t τ : term), term
| _app   : forall (t1 t2 : term), term
| _hole  : term
| _lam   : forall (b : binder) (t : term), term
| _let   : forall (b : binder) (t1 t2 : term), term
| _pi    : forall (b : binder) (τ1 τ2 : term), term
| _type  : term
| _var   : forall (v : variable), term
.

Inductive LocalDeclaration :=
| LocalAssum : forall (v : variable) (τ : term),            LocalDeclaration
| LocalDef   : forall (v : variable) (t : term) (τ : term), LocalDeclaration
.

Definition nameOfLocalDeclaration (ld : LocalDeclaration) : variable.
  destruct ld; exact v.
Defined.

Definition typeOfLocalDeclaration (ld : LocalDeclaration) : term.
  destruct ld; exact τ.
Defined.

Record Goal := MkGoal
  {   hypotheses : list (LocalDeclaration)
    ; conclusion : term
  }.

Inductive Atomic :=
| Intro : binder -> Atomic
.

Require Import List.
Import ListNotations.

Definition addHyp (hyp : LocalDeclaration) (hyps : list LocalDeclaration)
  : option (list LocalDeclaration).
  destruct (
      List.existsb
        (fun v' => if string_dec (nameOfLocalDeclaration hyp) v' then true else false)
        (List.map nameOfLocalDeclaration hyps)
    ).
  exact None.
  exact (Some (hyp :: hyps)).
Defined.

From Chick Require Import Applicative.
From Chick Require Import Monad.

Global Instance Functor_option : Functor option.
Proof.
  constructor.
  + exact option_map.
Defined.

Global Instance Applicative_option : Applicative option.
Proof.
  constructor.
  + auto with typeclass_instances.
  + exact @Some.
  + intros a b of oa.
    refine (
        match of, oa with
        | Some f, Some a => Some (f a)
        | _, _ => None
        end
      ).
Defined.

Global Instance Monad_option : Monad option.
Proof.
  constructor.
  + auto with typeclass_instances.
  + intros T oo.
    destruct oo.
    destruct o.
    exact (Some t).
    exact None.
    exact None.
Defined.

Axiom αrename : variable -> variable -> term -> term.
Axiom βeq : term -> term -> bool.
Axiom isFree : variable -> term -> bool.
Axiom typeOf : term -> term.
Axiom subst : variable -> term -> term -> term.

Definition LocalContext := list LocalDeclaration.

Definition isPi (t : term) : option (binder * term * term) :=
  match t with
  | _pi b τ1 τ2 => Some (b, τ1, τ2)
  | _ => None
  end.

Definition addLocalAssum (b : binder) (t : term) (Γ : LocalContext) : LocalContext.
  destruct b.
  + exact (LocalAssum v t :: Γ).
  + exact Γ.
Defined.

(* Coq's standard library sucks... *)
Definition hasVariable (v : variable) (l : LocalDeclaration) : bool.
  exact (if string_dec v (nameOfLocalDeclaration l) then true else false).
Defined.

Fixpoint
  synth
  (check : LocalContext -> term -> term -> option term)
  (Γ : LocalContext) (t : term) : option term.
  remember t as T.
  destruct t as [ | t1 t2 | | | | b τIn τOut | | ].
  + exact None.
  + refine (
        t1' <- synth check Γ t1 ;
        pi <- isPi t1' ;
        let (bτIn, τOut) := (pi : (binder * term * term)) in
        let (b, τIn) := bτIn in
        t2' <- check Γ t2 τIn ;
        match b with
        | Some v =>
          let τOut' := subst v t2' τOut in
          return_ τOut'
        | None =>
          return_ τOut
        end
      ).
  + exact None.
  + exact None.
  + exact None.
  + refine (
        τIn' <- check Γ τIn _type ;
        let Γ' := addLocalAssum b τIn' Γ in
        τOut' <- check Γ' τOut _type;
        return_ _type
      ).
  + exact (Some _type).
  + destruct (find (hasVariable v) Γ).
    { exact (Some (typeOfLocalDeclaration l)). }
    { exact None. }
Defined.

Fixpoint termMeasure (t : term) : nat.
  destruct t.
  + exact (S (termMeasure t1 + termMeasure t2)).
  + exact (S (termMeasure t1 + termMeasure t2)).
  + exact 0.
  + exact (S (termMeasure t)).
  + exact (S (termMeasure t1 + termMeasure t2)).
  + exact (S (termMeasure t1 + termMeasure t2)).
  + exact 0.
  + exact 0.
Defined.

Require Import Coq.Program.Wf.

Definition checkMeasure (tτ1 tτ2 : (term * term)) :=
  let (t1, τ1) := tτ1 in
  let (t2, τ2) := tτ2 in
    termMeasure t1 + termMeasure τ1
  < termMeasure t2 + termMeasure τ2.

(*
Lemma checkMeasure_wf' :
  forall m, forall t τ, termMeasure t + termMeasure τ <= m -> Acc checkMeasure (t, τ).
  unfold termMeasure.
  induction m; fold termMeasure.
  + intros.
    destruct t; try solve [inversion H];
    destruct τ; try solve [inversion H];
    constructor; intros [t τ]; inversion 1.
  + fold termMeasure in IHm.
    intros t τ H.
    apply IHm.
    destruct t.
    simpl in *.
    apply le_S_n in H.
    apply IHm.
    simpl.
    ring.
    tauto.
    intuition.
    auto.
    specialize (IHm  H).
    inversion H.


    ; try solve [inversion H].
    destruct τ; try solve [inversion H];
    constructor; intros [t τ]; inversion 1.
    unfold checkMeasure.
    simpl.

    auto.
    destruct t; destruct τ; simpl in H. inversion H.
  simpl.
  unfold lengthOrder; induction len; crush.
  Defined.

Lemma checkMeasure_wf : well_founded checkMeasure.
Proof.
  intros [t1 τ1].
  induction t1.
  constructor.
  unfold Acc.
  simpl in *.

  constructor.
  constructor.
  constructor.
  constructor.
  constructor.
  constructor.
  red.
  intro a. constructor. intro y.
  destruct a as [t1 τ1], y as [t2 τ2].
  simpl.
  constructor.


  intros y H. unfold checkMeasure in H.
  constructor.
  intr
 *)

Definition
  check_lam
  (check : SynthType -> CheckType) (synth : SynthType) (Γ : LocalContext)
  τ
  binderLam bodyLam :=
  τ' <- check synth Γ τ _type ;
  pi <- isPi τ' ;
  let '( (binderPi , τIn) , τOut) := (pi : (_ * _ * _)) in
  (* TODO: match binderLam and binderPi, αrename if necessary *)
  let Γ' := addLocalAssum binderLam τIn Γ in
  _ <- check synth Γ' bodyLam τOut;
    return_ τ'
.

Program
  Fixpoint
  check
  (synth : LocalContext -> term -> option term)
  (Γ : LocalContext) (t : term) (τ : term)
  { measure (termMeasure t + termMeasure τ) }
  : option term :=
  match t with
  | _lam binderLam bodyLam => check_lam check synth Γ τ binderLam bodyLam
  | _hole => None
  | _ =>
    t' <- synth Γ t ;
      if βeq t' τ then Some t' else None
  end.
Next Obligation.
simpl.

Fixpoint typeOf (t : term) : option term.
  remember t as T.
  destruct t.
  + exact (typeOf t2).
  +


  exact

Definition
  runIntro
  (hyps : list LocalDeclaration) (introed : term) (rest : term)
  (h : variable -> term -> LocalDeclaration)
  : (binder * binder) -> option Goal.
  refine (
      fun mimv =>
        match mimv with
        | (None,   None)   => Some (MkGoal hyps rest)
        | (Some i, None)   => MkGoal <$> addHyp (h i introed) hyps <*> pure rest
        | (None,   Some v) =>
          if isFree v rest then pure (MkGoal hyps rest) else None
        | (Some i, Some v) =>
          MkGoal <$> addHyp (h i introed) hyps <*> pure (αrename v i rest)
        end
    ).
Defined.

Definition runAtomic (a : Atomic) (g : Goal) : option Goal.
  destruct g as [hyps concl].
  destruct a as [mi].
  refine (
      match concl with
      | _let mv t1 t2 =>
        runIntro hyps (typeOf t1) t2 (fun v τ => LocalDef v t1 τ) (mi, mv)
      | _pi  mv τ1 τ2 =>
        runIntro hyps τ1 τ2 LocalAssum (mi, mv)
      | _ => None
      end
    ).


















(*
Fixpoint tcAnnot n : annot :=
  let t := (match n with O => False | S n' => term (tcAnnot n') end :   Type) in
  MkAnnot t t t t t t unit t.

Definition tcTerm := { n : nat & term (tcAnnot n) }.

Definition testing :=
  @_pi (tcAnnot 2)
       (_type (tcAnnot 1) tt)
       None
       (_type (tcAnnot 2) tt)
       (_type (tcAnnot 2) tt).

Definition typeOf (n : nat) (t : term (tcAnnot (S n))) : term (tcAnnot n).
  refine (
      match t with
      | _annot a _ τ   => a
      | _app   a _ _   => a
      | _hole  _ a       => a
      | _lam   a _ _   => a
      | _let   a _ _ _ => a
      | _pi    a _ _ _ => a
      | _type  _ a       => _type _ _
      | _var   _ a _     => a
      end
    ).
  destruct n; exact tt.
Defined.

Inductive LocalDeclaration a :=
| LocalAssum : forall (v : variable) (τ : term a),              LocalDeclaration a
| LocalDef   : forall (v : variable) (t : term a) (τ : term a), LocalDeclaration a
.

Definition nameOfLocalDeclaration ξ (ld : LocalDeclaration ξ) : variable.
  destruct ld; exact v.
Defined.

Record Goal a := MkGoal
  {   hypotheses : list (LocalDeclaration a)
    ; conclusion : term a
  }.

Inductive Atomic :=
| Intro : binder -> Atomic
.

Require Import List.
Import ListNotations.

Definition addHyp ξ (hyp : LocalDeclaration ξ) (hyps : list (LocalDeclaration ξ))
  : option (list (LocalDeclaration ξ)).
  destruct (
      List.existsb
        (fun v' => if string_dec (nameOfLocalDeclaration hyp) v' then true else false)
        (List.map (@nameOfLocalDeclaration _) hyps)
    ).
  exact None.
  exact (Some (hyp :: hyps)).
Defined.

From Chick Require Import Applicative.

Global Instance Functor_option : Functor option.
Proof.
  constructor.
  + exact option_map.
Defined.

Global Instance Applicative_option : Applicative option.
Proof.
  split.
  + auto with typeclass_instances.
  + exact @Some.
  + intros a b of oa.
    refine (
        match of, oa with
        | Some f, Some a => Some (f a)
        | _, _ => None
        end
      ).
Defined.

Axiom αrename : forall {ξ}, variable -> variable -> term ξ -> term ξ.
Axiom isFree : forall {ξ}, variable -> term ξ -> bool.

Definition
  runIntro
  ξ (hyps : list (LocalDeclaration ξ)) (introed : term ξ) (rest : term ξ)
  (h : variable -> term ξ -> LocalDeclaration ξ)
  : (binder * binder) -> option (Goal ξ).
  refine (
      fun mimv =>
        match mimv with
        | (None,   None)   => Some (MkGoal hyps rest)
        | (Some i, None)   => @MkGoal _ <$> addHyp (h i introed) hyps <*> pure rest
        | (None,   Some v) =>
          if isFree v rest then pure (MkGoal hyps rest) else None
        | (Some i, Some v) =>
          @MkGoal _ <$> addHyp (h i introed) hyps <*> pure (αrename v i rest)
        end
    ).
Defined.

Definition runAtomic n (a : Atomic) (g : Goal (tcAnnot (S n)))
  : option (Goal (tcAnnot n)).
  destruct g as [hyps concl].
  destruct a as [mi].
  refine (
      match concl with
      | _let _ mv t1 t2 => runIntro hyps (typeOf t1) _ _ _
      | _pi  _ mv τ1 τ2 => _
      | _ => None
      end
    ).
