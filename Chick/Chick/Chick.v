Set Implicit Arguments.
Unset Strict Implicit.

Require Import String.

Definition variable := string.
Definition binder := option variable.

Record metadata :=
  MkAnnot { ofApp : Type; ofLam : Type; ofType : Type; ofVar : Type }.

Inductive term (a : metadata) :=
| app   : ofApp   a -> term a -> term a -> term a
| lam   : ofLam   a -> binder -> term a -> term a
| type  : ofType  a -> term a
| var   : ofVar   a -> variable -> term a
.

Fixpoint tcMetadata n : metadata :=
  match n with
  | O => MkAnnot False False unit False
  | S n' => let t := term (tcMetadata n') in MkAnnot t t unit t
  end.

Fixpoint lift (n : nat) (t : term (tcMetadata n)) {struct t} : term (tcMetadata (S n)).
  refine (
  match n as _n return term (tcMetadata _n) -> term (tcMetadata (S _n)) with
  | O => fun _ => type (tt : ofType (tcMetadata 1))
  | S n' => fun t =>
    match t with
    | app   a t1 t2   => app _ (lift _ t1) (lift _ t2)
    | lam   a b t     => lam _ b (lift _ t)
    | type  a         => type _
    | var   a v       => var _ v
    end
  end t
    ).
  exact (lift _ a).
  exact (lift _ a).
  exact tt.
  exact (lift _ a).
Defined.

Inductive term (a : metadata) :=
| _annot : ofAnnot a -> term a -> term a -> term a
| _app   : ofApp   a -> term a -> term a -> term a
| _hole  : ofHole  a -> term a
| _lam   : ofLam   a -> binder -> term a -> term a
| _let   : ofLet   a -> binder -> term a -> term a -> term a
| _pi    : ofPi    a -> binder -> term a -> term a -> term a
| _type  : ofType  a -> term a
| _var   : ofVar   a -> variable -> term a
.

Record metadata :=
  MkAnnot
    {   ofAnnot : Type
      ; ofApp   : Type
      ; ofHole  : Type
      ; ofLam   : Type
      ; ofLet   : Type
      ; ofPi    : Type
      ; ofType  : Type
      ; ofVar   : Type
    }.

Inductive term (a : metadata) :=
| _annot : ofAnnot a -> term a -> term a -> term a
| _app   : ofApp   a -> term a -> term a -> term a
| _hole  : ofHole  a -> term a
| _lam   : ofLam   a -> binder -> term a -> term a
| _let   : ofLet   a -> binder -> term a -> term a -> term a
| _pi    : ofPi    a -> binder -> term a -> term a -> term a
| _type  : ofType  a -> term a
| _var   : ofVar   a -> variable -> term a
.

Fixpoint tcMetadata n : metadata :=
  match n with
  | O => MkAnnot False False False False False False unit False
  | S n' => let t := term (tcMetadata n') in MkAnnot t t t t t t unit t
  end.

Definition tcterm : Type := { m : nat & forall n, n >= m -> term (tcMetadata n) }.

Require Import Coq.Program.Wf.


Program Fixpoint lift (n : nat) (t : term (tcMetadata n)) {struct t}
  : bool :=
  match n with
  | O => true
  | S n' =>
      match t with
      | _annot a t τ => lift t
      | _ => false
      end
  end.
Next Obligation.

Fixpoint termMeasure (n : nat) (t : term (tcMetadata n)) : nat :=
  match t with
  | _annot a t τ     => _annot _ (lift t) (lift τ)
  | _app   a t1 t2   => _app _ (lift t1) (lift t2)
  | _hole  a         => _hole _
  | _lam   a b t     => _lam _ b (lift t)
  | _let   a b t1 t2 => _let _ b (lift t1) (lift t2)
  | _pi    a b τ1 τ2 => _pi _ b (lift τ1) (lift τ2)
  | _type  a         => _type _
  | _var   a v       => _var _ v
  end.

Program Fixpoint lift (n : nat) (t : term (tcMetadata n)) {measure n}
  : term (tcMetadata (S n)) :=
  match n with
  | O => _type tt
  | S n' =>
    match t with
    | _annot a t τ     => _annot _ (lift t) (lift τ)
    | _app   a t1 t2   => _app _ (lift t1) (lift t2)
    | _hole  a         => _hole _
    | _lam   a b t     => _lam _ b (lift t)
    | _let   a b t1 t2 => _let _ b (lift t1) (lift t2)
    | _pi    a b τ1 τ2 => _pi _ b (lift τ1) (lift τ2)
    | _type  a         => _type _
    | _var   a v       => _var _ v
    end
  end.
Next Obligation.

Definition typeOf (t : tcterm) : tcterm.
  destruct t as [m ?].
  exists (S m).
  intros n NSM.
  assert (n >= m) as NM by admit.
  specialize (t n NM).
  destruct n.
  admit.
  refine (
      match t with
      | _annot a _ τ   => _
      | _app   a _ _   => _
      | _hole  a       => _
      | _lam   a _ _   => _
      | _let   a _ _ _ => _
      | _pi    a _ _ _ => _
      | _type  a       => _type _
      | _var   a _     => _
      end
    ).
  simpl in a.
  exact a.
  destruct n; exact tt.
Defined.

(*
Definition noSuchTerm (t : term (tcMetadata 0))
  : t = _type (tt : ofType (tcMetadata 0)).
Proof.
  destruct t; destruct o. reflexivity.
Qed.
 *)

Definition typeOf (n : nat) (t : term (tcMetadata (S n))) : term (tcMetadata n).
  refine (
      match t with
      | _annot a _ τ   => a
      | _app   a _ _   => a
      | _hole  a       => a
      | _lam   a _ _   => a
      | _let   a _ _ _ => a
      | _pi    a _ _ _ => a
      | _type  a       => _type _
      | _var   a _     => a
      end
    ).
  destruct n; exact tt.
Defined.

(*
Definition tcTerm := { n : nat & term (tcAnnot n) }.

Definition typeOf (t : tcTerm) : tcTerm.
  destruct t as [n t].
  destruct n.
  destruct (noSuchTerm t).
  destruct t; try (exists n; exact o).
  exists (S n).
  refine (_type _).
  exact tt.
Defined.
 *)

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

Fixpoint lift (n : nat) (t : term (tcMetadata n)) {struct t}
  : term (tcMetadata (S n)).
  destruct n.
  refine (_type _). exact tt.
  refine (
      match t with
      | _annot a t τ     => _annot _ _ _
      | _app   a t1 t2   => _app _ (lift _ t1) (lift _ t2)
      | _hole  a         => _hole _
      | _lam   a b t     => _lam _ b (lift _ t)
      | _let   a b t1 t2 => _let _ b (lift _ t1) (lift _ t2)
      | _pi    a b τ1 τ2 => _pi _ b (lift _ τ1) (lift _ τ2)
      | _type  a         => _type _
      | _var   a v       => _var _ v
      end
    ); try exact (lift _ a).
  simpl in a.
  exact tt.
Defined.

Definition runAtomic n (a : Atomic) (g : Goal (tcMetadata (S n)))
  : option (Goal (tcMetadata (S n))).
  destruct g as [hyps concl].
  destruct a as [mi].
  refine (
      match concl with
      | _let _ mv t1 t2 =>
        runIntro hyps (typeOf t1) t2 (fun v τ => LocalDef v t1 τ) (mi, mv)
      | _pi  _ mv τ1 τ2 =>
        runIntro hyps τ1 τ2 (@LocalAssum _) (mi, mv)
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
