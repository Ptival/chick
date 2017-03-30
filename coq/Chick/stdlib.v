Require Import List.

From Chick Require Import inductive.

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
    [
      ([], mkVar "n")
      ;
      ([], mkVar "foo")
      ;
      (["n"], mkVar "n")
    ].

Require Import SquiggleEq.export.

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
