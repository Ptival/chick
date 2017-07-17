From Coq Require Import
     EquivDec
     String
.

From HaysTac Require Import HaysTac.

Definition variable := string.

Global Instance EqDec_goal : EqDec variable eq.
Proof string_dec.
