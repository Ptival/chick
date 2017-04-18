From Coq Require Import
     EquivDec
     List
.

From Chick Require Export
     Binder
.

From HaysTac Require Import
     HaysTac
.

From SquiggleEq Require Import
     export
.

Import ListNotations.

(*
Inductive term : Type :=
| App   : forall (t1 t2 : term), term
| Lam   : forall (b : binder) (t : term), term
| Pi    : forall (b : binder) (τ1 τ2 : term), term
| Type0 : term
| Var   : forall (v : variable), term
.
*)

Inductive termC : Type :=
| App   : termC
| Hole  : termC
| Lam   : termC
| Match : list nat -> termC
| Pi    : bool -> termC
| Type' : termC
.

Definition termOpBindings (t : termC) : list nat :=
  match t with
  | App      => [0; 0]
  | Hole     => []
  | Lam      => [1]
  | Match l  => l
  | Pi false => [0; 0]
  | Pi true  => [0; 1]
  | Type'    => []
  end.

Global Instance termOpid : GenericTermSig termC.
Proof.
  constructor.
  exact termOpBindings.
Defined.

Require Import String.

Global Instance varClassString : VarClass variable string.
Proof.
  constructor.
Defined.

Global Instance freshVarsString : FreshVars variable string.
Proof.
  constructor.
Defined.

Definition term := @NTerm variable termC.

Definition
  termSubst
  (t : term) (sub : @Substitution variable termC)
  : term
  := ssubst t sub.

Open Scope string.

Definition mkApp (t1 t2 : term) : term
  := oterm App [bterm [] t1; bterm [] t2].

Definition mkHole : term
  := oterm Hole [].

Definition mkLam (v : variable) (body : term) : term
  := oterm Lam [bterm [v] body].

Definition mkPi (b : binder) (τ1 τ2 : term) : term
  :=
    match b with
    | None   => oterm (Pi false) [bterm [] τ1; bterm []  τ2]
    | Some v => oterm (Pi true)  [bterm [] τ1; bterm [v] τ2]
    end.

Definition mkType : term
  := oterm Type' [].

Definition mkVar (v : variable) : term
  := vterm v.

Theorem isprogram_mkLam :
  forall v body,
    list.subset (free_vars body) [v] ->
    nt_wf body ->
    isprogram (mkLam v body).
Proof.
  intros v body FV WF.
  constructor.
  {
    unfold closed.
    simpl.
    rewrite app_nil_r.
    apply list.null_iff_nil.
    do 2 intro.
    apply_in_hyp list.in_remove.
    on and destruct'.
    on @list.subset ltac:(repeat' find_specialize_in).
    on In inversion'.
    + congruence.
    + on In inversion'.
  }
  {
    constructor.
    {
      intros.
      repeat on In inversion'; now constructor.
    }
    { reflexivity. }
  }
Qed.

Theorem nt_wf_mkPi :
  forall b τ1 τ2,
    nt_wf τ1 ->
    nt_wf τ2 ->
    nt_wf (mkPi b τ1 τ2).
Proof.
  intros.
  on binder destruct'.
  {
    constructor.
    {
      intros.
      repeat on In inversion'; now constructor.
    }
    { reflexivity. }
  }
  {
    constructor.
    {
      intros.
      repeat on In inversion'; now constructor.
    }
    { reflexivity. }
  }
Qed.

Global Instance EqDec_termC : EqDec termC eq.
Proof with decide_equality.
  repeat intro.
  do 2 on termC destruct'...
  - on2 list destruct_EqDec...
  - on2 bool destruct_EqDec...
Defined.

Fixpoint EqDec_NTerm'
         {NVar Opid} `{EqDec NVar eq} `{EqDec Opid eq} (x y : @NTerm NVar Opid)
  : {x === y} + {x =/= y}
with EqDec_BTerm'
       {NVar Opid} `{EqDec NVar eq} `{EqDec Opid eq} (x y : @BTerm NVar Opid)
     : {x === y} + {x =/= y}
.
Proof with decide_equality.
  - clear EqDec_NTerm'.
    do 2 on @NTerm destruct'...
    + on2 NVar destruct_EqDec...
    + on2 Opid destruct_EqDec...
      * on_head2 list ltac:(
          fun l1 l2 =>
            eapply List.list_eq_dec with (l := l1) (l' := l2) in EqDec_BTerm'
        ); eauto.
        on sumbool destruct'...
  - clear EqDec_BTerm'.
    do 2 on @BTerm destruct'...
    + on2 @NTerm ltac:(
        fun n1 n2 =>
          edestruct EqDec_NTerm' with (x := n1) (y := n2)
      ); eauto...
      on_head (EqDec NVar) ltac:(fun H => pose proof (List.list_eq_dec H)).
      on_head2 list ltac:(
        fun l1 l2 =>
          let H := enumerate_hypotheses in
          specialize (H l1 l2)
      ).
      on_head sumbool destruct'...
Defined.

Global Instance EqDec_NTerm :
  forall {NVar Opid} `{EqDec NVar eq} `{EqDec Opid eq}, EqDec (@NTerm NVar Opid) eq.
Proof.
  do 6 intro.
  exact EqDec_NTerm'.
Defined.

Global Instance EqDec_BTerm :
  forall {NVar Opid} `{EqDec NVar eq} `{EqDec Opid eq}, EqDec (@BTerm NVar Opid) eq.
Proof.
  do 6 intro.
  exact EqDec_BTerm'.
Defined.

Global Instance EqDec_term : EqDec term eq.
Proof with decide_equality.
  do 2 intro.
  do 2 on term destruct'...
  - on2 variable destruct_EqDec...
  - on_head2 termC destruct_EqDec...
    on_head2 list destruct_EqDec...
Defined.
