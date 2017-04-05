From Coq Require Import List.

From SquiggleEq Require Import export.

From HaysTac Require Import HaysTac.

From Chick Require Export inductive.

Import ListNotations.

Definition boolInductive : inductive :=
  Inductive'
    "bool" [] []
    [
      Constructor "true"  [] []
      ;
      Constructor "false" [] []
    ]
.

Definition natInductive : inductive :=
  Inductive' "nat" [] []
    [
      Constructor "O" [] []
      ;
      Constructor
        "S"
        [(Some "n", mkVar "nat")]
        []
    ]
.

Definition listInductive : inductive :=
  Inductive'
    "list"
    [
      (Some "A", mkType)
    ]
    []
    [
      Constructor "nil" [] []
      ;
      Constructor
        "cons"
        [
          (Some "x", mkVar "T")
          ;
          (Some "xs", mkApp (mkVar "list") (mkVar "T"))
        ]
        []
    ]
.

Definition testing :=
  mkMatch
    natInductive
    (mkVar "n")
    [
      ([], mkVar "foo")
      ;
      (["n"], mkVar "n")
    ].

Theorem nt_wf_testing : nt_wf testing.
Proof.
  constructor.
  {
    intros l IN.
    simpl in IN.
    destruct IN as [IN | [IN | [IN | ]]]; subst.
    { repeat constructor. }
    { repeat constructor. }
    { repeat constructor. }
    { intuition. }
  }
  { reflexivity. }
Qed.
