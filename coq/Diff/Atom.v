Set Implicit Arguments.

Inductive Δ (τ : Type) : Type :=
| Same    :      Δ τ
| Replace : τ -> Δ τ
.
Arguments Same [τ].

Definition patch τ (δ : Δ τ) : τ -> option τ :=
  match δ with
  | Same      => Some
  | Replace r => fun _ => Some r
  end.

(*
Global Instance Desc_Δ τ : Desc (Δ τ) :=
  {
    To := τ -> τ;
    denote δ :=
      match δ with
      | Same      => id
      | Replace r => fun _ => r
      end;
  }.
*)
