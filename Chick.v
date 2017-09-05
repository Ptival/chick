Set Implicit Arguments.

From Coq Require Import
     List
     Program
     String
     Unicode.Utf8
.

Import ListNotations.
Open Scope string.

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
Notation "[ a ]" := (ListD a).

Check List.fold_left.

Inductive OpD : TypeD -> Type :=
| EvenD : OpD ( NatD --> BoolD )
| FoldLeftD :
    ∀ (A B : TypeD) (f : OpD (A --> B --> A)) (base : denoteTypeD A),
      OpD ( [ B ] --> A )
| MapD :
    ∀ (t1 t2 : TypeD) (f : OpD (t1 --> t2)),
      OpD ( [ t1 ] --> [ t2 ] )
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

Module δAtom.

  Inductive D (τ : Type) : Type :=
  | Same    :      D τ
  | Replace : τ -> D τ
  .
  Arguments Same [τ].

  Definition patch τ (d : D τ) : τ -> option τ :=
    match d with
    | Same      => Some
    | Replace r => fun _ => Some r
    end.

  Global Instance Desc_D τ : Desc (D τ) :=
    {
      To := τ -> τ;
      denote d :=
        match d with
        | Same      => id
        | Replace r => fun _ => r
        end;
    }.

End δAtom.

Module δList.

  Inductive D (τ : Type) : Type :=
  | Same   : D τ
  | Insert : ∀ (h  : τ)         (δt : D τ), D τ
  | Modify : ∀ (δh : δAtom.D τ) (δt : D τ), D τ
  .
  Arguments Same [τ].

  Fixpoint patch τ (d : D τ) : list τ -> option (list τ) :=
    match d with
    | Same         => Some
    | Insert h  δt => fun l => option_map (cons h) (patch δt l)
    | Modify δh δt =>
      fun l =>
        match l with
        | []     => None
        | h :: t => option_map (cons (⟦ δh ⟧ h)) (patch δt t)
        end
    end.

  Global Instance Desc_D τ : Desc (D τ) :=
    {
      To := list τ -> option (list τ);
      denote := @patch τ;
    }.

End δList.

Module δTerm.

  Inductive δ : Type :=
  | Same : δ
  .

End δTerm.

Definition insert42 := ⟦ δList.Insert 42 δList.Same ⟧.

Theorem insert42works : insert42 [1; 2; 3] = Some [42; 1; 2; 3].
Proof.
  reflexivity.
Qed.

Theorem mapNatBoolworks :
  ⟦ MapD EvenD ⟧ [0; 1; 2; 3] = [true; false; true; false].
Proof.
  reflexivity.
Qed.

Theorem thm1 :
  ∀ T (x : T) l,
    ⟦ δList.Insert x δList.Same⟧ l = Some (x :: l).
Proof.
  reflexivity.
Qed.

Theorem thm2 :
  ∀
    (A B : TypeD) (f : OpD (A --> B)) x l,
    Some (⟦ MapD f ⟧ (x :: l)) = Some (⟦ f ⟧ x :: ⟦ MapD f ⟧ l).
Proof.
  induction l.
  + reflexivity.
  + now reflexivity.
Qed.

Theorem option_map_compose :
  ∀ A B C (f1 : A -> B) (f2 : B -> C) x,
    option_map f2 (option_map f1 x) = option_map (f2 ∘ f1) x.
Proof.
  now destruct x.
Qed.

Theorem thm3 :
  ∀ (A B : TypeD) (f : OpD (A --> B)) (x : denoteTypeD A) δl l,
    option_map
      ⟦ MapD f ⟧
      (⟦ δList.Insert x δl ⟧ l)
    =
    option_map
      (cons (⟦ f ⟧ x) ∘ ⟦ MapD f ⟧)
      (⟦ δl ⟧ l).
Proof.
  induction l; simpl; now rewrite option_map_compose. (* damn Coq! *)
Qed.

Record δ (d : TypeD) :=
  {
    δτ : Type;
    patch : δτ -> denoteTypeD d -> option ⟦ d ⟧
  }.

(*
Definition δArrowD d1 d2 : δ (ArrowD d1 d2).
  refine {| δτ := False; patch := _; |}.
  destruct 1.
Defined.
*)

Definition δBoolD : δ BoolD.
  apply Build_δ with (δτ := δAtom.D bool).
  apply δAtom.patch.
Defined.

Definition Variable' := string.
Definition Binder := option Variable'.

Inductive Term (ν : Type) : Type :=
| App   : ∀ (t1 t2 : Term ν), Term ν
| Pi    : ∀ (τ1 : Term ν) (b : Binder) (τ2 : Term ν), Term ν
| Type' : Term ν
| Var   : ∀ (v : ν), Term ν
.
Arguments Type' [ν].

Definition mkPi '(b, τ) a : Term Variable' := Pi τ b a.

Definition foldrWith T A f (l : list T) (a : A) := List.fold_right f a l.

Definition quantifyBinders := foldrWith mkPi.

Definition quantifyVariables l : Term Variable' -> Term Variable' :=
  quantifyBinders (List.map (fun '(v, τ) => (Some v, τ)) l).

Notation "f $ x" := (f x) (right associativity, at level 180, only parsing).

Definition VTerms := list (Variable' * Term Variable').

Definition mkMotiveType' indName (indParams indIndices : VTerms) universe :=

  let onIndIndexOutside '(v, p) t       := Pi  p (Some v) t      in
  let onIndParam        t       '(b, _) := App t (Var b) in
  let onIndIndexInside  t       '(v, _) := App t (Var v) in

  let inductive :=
    List.fold_left
      onIndIndexInside
      indIndices
      (List.fold_left
         onIndParam
         indParams
         (Var indName)
      )
  in

  List.fold_right
    onIndIndexOutside
    (Pi inductive None universe)
    indIndices
.

Definition mkCase motive consName consParameters consIndices :=
  quantifyBinders (concatMap addRecursiveMotive consParameters)
                  $ applyTerm [applyVar consParameters (Var Nothing consName)]
                  $ applyTerm consIndices
                  $ motive.

Definition quantifyCase motive (constructor : (Variable' * VTerms * list (Term Variable'))) acc :=
  let '(consName, consParameters, consIndices) := constructor in
  Pi (mkCase motive consName consParameters consIndices) None acc.

Definition quantifyCases motive (constructors : list (Variable' * VTerms * list (Term Variable'))) :=
  foldrWith (quantifyCase motive) constructors.

Definition eliminatorType'
           (inductiveName       : Variable')
           (inductiveParameters : VTerms)
           (inductiveIndices    : VTerms)
           (constructors        : list (Variable' * VTerms * list (Term Variable')))
  :=

  let motive := "Motive" in
  let motiveType := mkMotiveType' inductiveName inductiveParameters inductiveIndices Type' in

    quantifyVariables inductiveParameters
  $ quantifyVariables ([(motive, motiveType)])
  $ quantifyCases motive constructors
  $ quantifyVariables inductiveIndices
  $ quantifyVariables [(discriminee, discrimineeType)]
  $ outputType
.
