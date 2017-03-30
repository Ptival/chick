Require Import List.
Require Import SquiggleEq.export.

Require Export Chick.binder.

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
    intros x Hx.
    apply list.in_remove in Hx.
    destruct Hx as [Hx1 Hx2].
    specialize (FV x Hx2).
    inversion FV as [|IN].
    + congruence.
    + inversion IN.
  }
  {
    constructor.
    {
      intros l IN.
      inversion_clear IN as [|IN'].
      {
        subst.
        constructor.
        assumption.
      }
      { inversion IN'. }
    }
    {
      reflexivity.
    }
  }
Qed.

Theorem nt_wf_mkPi :
  forall b τ1 τ2,
    nt_wf τ1 ->
    nt_wf τ2 ->
    nt_wf (mkPi b τ1 τ2).
Proof.
  intros b τ1 τ2 WF1 WF2.
  destruct b.
  {
    constructor.
    {
      intros l IN.
      inversion_clear IN as [|[|IN']].
      { subst. constructor. assumption. }
      { subst. constructor. assumption. }
      { inversion IN'. }
    }
    {
      reflexivity.
    }
  }
  {
    constructor.
    {
      intros l IN.
      inversion_clear IN as [|[|IN']].
      { subst. constructor. assumption. }
      { subst. constructor. assumption. }
      { inversion IN'. }
    }
    {
      unfold num_bvars. simpl.
      reflexivity.
    }
  }
Qed.
