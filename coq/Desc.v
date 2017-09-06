
Class Desc (From : Type) :=
  {
    To : Type;
    denote : From -> To;
  }.

Notation "⟦ d ⟧" := (denote d).

Inductive TypeD : Type :=
| ArrowD : TypeD -> TypeD -> TypeD
| BoolD  : TypeD
| ListD  : TypeD -> TypeD
| NatD   : TypeD
.

Fixpoint denoteTypeD d :=
  match d with
  | ArrowD dL dR => denoteTypeD dL -> denoteTypeD dR
  | BoolD        => bool
  | ListD  dT    => list (denoteTypeD dT)
  | NatD         => nat
  end.

Global Instance Desc_TypeD : Desc TypeD:=
  {
    To := Type;
    denote := denoteTypeD;
  }.

Notation "a --> b" := (ArrowD a b) (at level 99, right associativity).
Notation "[ a ]d" := (ListD a).

Check List.fold_left.

Inductive OpD : TypeD -> Type :=
| EvenD : OpD ( NatD --> BoolD )
| FoldLeftD :
    ∀ (A B : TypeD) (f : OpD (A --> B --> A)) (base : denoteTypeD A),
      OpD ( [ B ]d --> A )
| MapD :
    ∀ (t1 t2 : TypeD) (f : OpD (t1 --> t2)),
      OpD ( [ t1 ]d --> [ t2 ]d )
.

Fixpoint denote_OpD (d : TypeD) (opD : OpD d) : denoteTypeD d :=
  match opD with
  | EvenD         => Nat.even
  | FoldLeftD f b => fun l => List.fold_left (denote_OpD f) l b
  | MapD      f   => List.map (denote_OpD f)
  end.

Global Instance Desc_OpD d : Desc (OpD d) :=
  {
    To := ⟦ d ⟧;
    denote := @denote_OpD d;
  }.

Definition mapType t1 t2 := (t1 -> t2) -> list t1 -> list t2.

Theorem JustChecking :
  ∀ A B (f : OpD (A --> B)) l,
    ⟦ MapD f ⟧ l = List.map ⟦ f ⟧ l.
Proof.
  reflexivity.
Qed.

(*
Record Δ (D : TypeD) :=
  {
    δτ : Type;
    patch : δτ -> denoteTypeD D -> option ⟦ D ⟧
  }.

Definition δBoolD : Δ BoolD.
  apply Build_Δ with (δτ := δAtom.Δ bool).
  apply δAtom.patch.
Defined.
*)
