Set Implicit Arguments.

From Coq Require Import
     Ascii
     List
     NPeano
     Program
     Recdef
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

Module δAtom.

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

  Global Instance Desc_Δ τ : Desc (Δ τ) :=
    {
      To := τ -> τ;
      denote δ :=
        match δ with
        | Same      => id
        | Replace r => fun _ => r
        end;
    }.

End δAtom.

Notation "f <$> x" := (option_map f x) (at level 60).

Module δList.

  Inductive Δ (τ : Type) (Δτ : Type) : Type :=
  | Insert : ∀ (h  : τ)  (Δt : Δ τ Δτ), Δ τ Δτ
  | Keep   : ∀ (Δt : Δ τ Δτ), Δ τ Δτ
  | Modify : ∀ (Δh : Δτ) (Δt : Δ τ Δτ), Δ τ Δτ
  | Same   : Δ τ Δτ
  .
  Arguments Same [τ Δτ].

  Fixpoint patch τ Δτ (patchτ : Δτ -> τ -> option τ)
           (δ : Δ τ Δτ) : list τ -> option (list τ) :=
    match δ with

    | Insert h  δt => fun l => cons h <$> patch patchτ δt l

    | Keep δt =>
      fun l =>
        match l with
        | h :: t => cons h <$> patch patchτ δt t
        | _ => None
        end

    | Modify δh δt =>
      fun l =>
        match l with
        | []     => None
        | h :: t =>
          match (patchτ δh h, patch patchτ δt t) with
          | (Some h', Some t') => Some (h' :: t')
          | _ => None
          end
        end

    | Same         => Some

    end.

End δList.

Record Δ (D : TypeD) :=
  {
    δτ : Type;
    patch : δτ -> denoteTypeD D -> option ⟦ D ⟧
  }.

Definition δBoolD : Δ BoolD.
  apply Build_Δ with (δτ := δAtom.Δ bool).
  apply δAtom.patch.
Defined.

Definition Variable' := string.
Definition Binder ν := option ν.

Inductive Term (ν : Type) : Type :=
| App   : ∀ (t1 t2 : Term ν), Term ν
| Pi    : ∀ (τ1 : Term ν) (b : Binder ν) (τ2 : Term ν), Term ν
| Type' : Term ν
| Var   : ∀ (v : ν), Term ν
.
Arguments Type' [ν].

Definition mkApp a t : Term Variable' := App a t.

Definition mkPi '(b, τ) a : Term Variable' := Pi τ b a.

Definition foldlWith T A f (l : list T) (a : A) := List.fold_left f l a.

Definition foldrWith T A f (l : list T) (a : A) := List.fold_right f a l.

Definition applyTerm := foldlWith mkApp.

Definition VTerm ν := (ν * Term ν)%type.
Definition VTerms ν := list (VTerm ν).
Definition BTerm ν := (Binder ν * Term ν)%type.
Definition BTerms ν := list (BTerm ν).

Definition applyVar (l : VTerms Variable') : Term Variable' -> Term Variable' :=
  applyTerm (List.map (@Var _ ∘ fst) l).

Definition quantifyBinders := foldrWith mkPi.

Definition quantifyVariables l : Term Variable' -> Term Variable' :=
  quantifyBinders (List.map (fun '(v, τ) => (Some v, τ)) l).

Notation "f $ x" := (f x) (right associativity, at level 180, only parsing).

Fixpoint unpackFullyAppliedInductive' inductiveName term nbParams nbIndices acc :=
  match (term, nbParams, nbIndices) with
  | (Var v, 0, 0) =>
    if string_dec v inductiveName
    then Some acc
    else None
  | (_, 0, 0) => None
  | (App l _, _, 0) =>
    unpackFullyAppliedInductive' inductiveName l (nbParams - 1) nbIndices       acc
  | (App l r, _, _) =>
    unpackFullyAppliedInductive' inductiveName l nbParams       (nbIndices - 1) (r :: acc)
  | (_, _, _) => None
  end.

Definition unpackFullyAppliedInductive
           (inductiveName : string)
           (inductiveParameters : VTerms Variable')
           (inductiveIndices : VTerms Variable')
           t :=
  unpackFullyAppliedInductive' inductiveName t
                               (List.length inductiveParameters)
                               (List.length inductiveIndices)
                               [].

Definition addRecursiveMotive
           inductiveName inductiveParameters inductiveIndices motive (vτ : VTerm Variable')
  : BTerms Variable' :=
  let '(v, τ) := vτ in
  match unpackFullyAppliedInductive inductiveName inductiveParameters inductiveIndices τ with
  | Some indices =>
    [ (Some v, τ)
    ; (None, App (applyTerm indices motive) (Var v))
    ]
  | None => [(Some v, τ)]
  end.

Definition mkCase
           inductiveName inductiveParameters inductiveIndices motive consName consParameters
           consIndices :=
  quantifyBinders (flat_map
                     (addRecursiveMotive inductiveName inductiveParameters inductiveIndices motive)
                     consParameters)
                  $ applyTerm [applyVar consParameters (Var consName)]
                  $ applyTerm consIndices
                  $ motive.

Definition quantifyCase
           inductiveName inductiveParameters inductiveIndices motive
           (constructor : (Variable' * VTerms Variable' * list (Term Variable'))) acc :=
  let '(consName, consParameters, consIndices) := constructor in
  Pi (mkCase inductiveName inductiveParameters inductiveIndices motive
             consName consParameters consIndices) None acc.

Definition quantifyCases
           inductiveName inductiveParameters inductiveIndices motive
           (constructors : list (Variable' * VTerms Variable' * list (Term Variable'))) :=
  foldrWith (quantifyCase inductiveName inductiveParameters inductiveIndices motive) constructors.

Definition mkMotiveType' indName (indParams indIndices : VTerms Variable') universe :=

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

Definition eliminatorType'
           (inductiveName       : Variable')
           (inductiveParameters : VTerms Variable')
           (inductiveIndices    : VTerms Variable')
           (constructors        : list (Variable' * VTerms Variable' * list (Term Variable')))
  :=

    let discriminee := "instance" in
    let discrimineeTerm := Var discriminee in
    let discrimineeType := applyVar inductiveIndices
                                    $ applyVar inductiveParameters
                                    $ Var inductiveName
    in

    let motive := "Motive" in
    let motiveTerm := Var motive in
    let motiveType := mkMotiveType' inductiveName inductiveParameters inductiveIndices Type' in

    let outputType := App (applyVar inductiveIndices motiveTerm) discrimineeTerm in

    quantifyVariables inductiveParameters
                      $ quantifyVariables ([(motive, motiveType)])
                      $ quantifyCases inductiveName inductiveParameters inductiveIndices motiveTerm
                      constructors
                      $ quantifyVariables inductiveIndices
                      $ quantifyVariables [(discriminee, discrimineeType)]
                      $ outputType
.

Inductive Constructor ν :=
  { constructorName       : ν
  ; constructorParameters : BTerms ν
  ; constructorIndices    : list (Term ν)
  }.

Inductive Inductive' ν :=
  { inductiveName         : ν
  ; inductiveParameters   : VTerms ν
  ; inductiveIndices      : BTerms ν
  ; inductiveConstructors : list (Constructor ν)
  }.

Definition mapWithIndex (A B : Type) (f : A -> nat -> B) (l : list A) : list B :=
  fst $ List.fold_right (fun a '(acc, i) => (f a i :: acc, i + 1)) ([], 0) l.

Definition digit_of_nat n := ascii_of_nat (n + 48).

Function string_of_nat_aux n acc {measure (fun x => x) n} :=
  match n with
    | 0 => acc
    | _ => string_of_nat_aux (n / 10) (String (digit_of_nat (n mod 10)) acc)
  end.
Proof.
  intros. apply Nat.div_lt; auto with arith.
Defined.

Definition string_of_nat n :=
  match n with
    | 0 => "0"%string
    | _ => string_of_nat_aux n EmptyString
  end.

Definition instantiateBinders prefix : BTerms Variable' -> VTerms Variable' :=
  let instantiate x i :=
      match x with
      | (Some v , τ) => (v, τ)
      | (None,    τ) => (String.append prefix (string_of_nat i), τ)
      end
  in
  mapWithIndex instantiate.


Definition instantiateConstructor '(Build_Constructor cn cps cis) :=
  (cn, instantiateBinders "p" cps, cis).

Definition instantiateConstructors := List.map instantiateConstructor.

Definition eliminatorType (ind : Inductive' Variable') : Term Variable' :=
  let '(Build_Inductive' indName indParams indIndices indConstructors) := ind in

  eliminatorType' indName indParams
                  (instantiateBinders "i" indIndices)
                  (instantiateConstructors indConstructors).

Module δPair.

  Inductive Δ (Δl Δr : Type) : Type :=
  | Same   : Δ Δl Δr
  | Modify : ∀ (δl : Δl) (δr : Δr), Δ Δl Δr
  .
  Arguments Same [Δl Δr].

  Definition patch Δl Δr
    (L : Type) (patchL : Δl -> L -> option L)
    (R : Type) (patchR : Δr -> R -> option R)
    (δ : Δ Δl Δr) : (L * R) -> option (L * R) :=
    match δ with
    | Same         => Some
    | Modify δl δr =>
      fun '(l, r) =>
        match (patchL δl l, patchR δr r) with
        | (Some l', Some r') => Some (l', r')
        | _ => None
        end
    end.

End δPair.

Module δTerm.

  Inductive Δ (ν : Type) : Type :=
  | CpyApp  : ∀ (δl : Δ ν) (δr : Δ ν), Δ ν
  | CpyPi   : ∀ (δl : Δ ν) (δb : δAtom.Δ (Binder ν)) (δr : Δ ν), Δ ν
  | InsPi   : ∀ (δl : Δ ν) (b : Binder ν) (δr : Δ ν), Δ ν
  | Replace : ∀ (r : Term ν), Δ ν
  | Same    : Δ ν
  .
  Arguments Same [ν].

  Fixpoint patch ν (δ : Δ ν) : Term ν -> option (Term ν) :=
    match δ with

    | CpyApp δl δr =>
      fun t =>
        match t with
        | App l r =>
          match (patch δl l, patch δr r) with
          | (Some l', Some r') => Some (App l' r')
          | _ => None
          end
        | _ => None
        end

    | CpyPi δl δb δr =>
      fun t =>
        match t with
        | Pi l b r =>
          match (patch δl l, δAtom.patch δb b, patch δr r) with
          | (Some l', Some b', Some r') => Some (Pi l' b' r')
          | _ => None
          end
        | _ => None
        end

    | InsPi δl b δr =>
      fun t =>
        match (patch δl t, patch δr t) with
        | (Some l', Some r') => Some (Pi l' b r')
        | _ => None
        end

    | Replace r => fun _ => Some r

    | Same => Some

    end.

End δTerm.

Module δConstructor.

  Definition Δcn := δAtom.Δ Variable'.

  Definition τcp := BTerm Variable'.
  Definition Δcp := δPair.Δ (δAtom.Δ (Binder Variable')) (δTerm.Δ Variable').
  Definition Δcps := δList.Δ τcp Δcp.

  Definition τci := Term Variable'.
  Definition Δci := δTerm.Δ Variable'.
  Definition Δcis := δList.Δ τci Δci.

  Inductive Δ :=
  | Same : Δ
  | Modify : ∀ (δcn : Δcn) (δcps : Δcps) (δcis : Δcis), Δ
  .

  Definition patchName (δcn : Δcn) cn := δAtom.patch δcn cn.

  Definition patchParameter : Δcp -> τcp -> option τcp :=
    δPair.patch (@δAtom.patch _) (@δTerm.patch _).

  Definition patchParameters (δcps : Δcps) cps :=
    δList.patch patchParameter δcps cps.

  Definition patchIndex : Δci -> τci -> option τci := @δTerm.patch _.

  Definition patchIndices (δcis : Δcis) cis :=
    δList.patch patchIndex δcis cis.

  Definition patch (δ : Δ) : Constructor Variable' -> option (Constructor Variable') :=
    match δ with
    | Same => Some
    | Modify δcn δcps δcis =>
      fun '(Build_Constructor cn cps cis) =>
        match (patchName δcn cn, patchParameters δcps cps, patchIndices δcis cis) with
        | (Some cn', Some cps', Some cis') => Some (Build_Constructor cn' cps' cis')
        | _ => None
        end
    end.

End δConstructor.

Definition concatListOption A (l : list (option A)) : option (list A) :=
  List.fold_right
    (fun oe ol =>
       match (ol, oe) with
       | (Some l, Some e) => Some (e :: l)
       | _ => None
       end
    ) (Some []) l.

Module δInductive.

  Definition Δn := δAtom.Δ Variable'.

  Definition τp := VTerm Variable'.
  Definition Δp := δPair.Δ (δAtom.Δ Variable') (δTerm.Δ Variable').
  Definition Δps := δList.Δ τp Δp.

  Definition τi := BTerm Variable'.
  Definition Δi := δPair.Δ (δAtom.Δ (Binder Variable')) (δTerm.Δ Variable').
  Definition Δis := δList.Δ τi Δi.

  Definition τc := Constructor Variable'.
  Definition Δc := δConstructor.Δ.
  Definition Δcs := δList.Δ τc Δc.

  Inductive Δ :=
  | Same : Δ
  | Modify : ∀ (δn : Δn) (δps : Δps) (δis : Δis) (δcs : Δcs), Δ
  .

  Definition patchName (δn : Δn) n := δAtom.patch δn n.

  Definition patchParameter : Δp -> τp -> option τp :=
    δPair.patch (@δAtom.patch _) (@δTerm.patch _).
  Definition patchParameters (δps : Δps) ps :=
    δList.patch patchParameter δps ps.

  Definition patchIndex : Δi -> τi -> option τi :=
    δPair.patch (@δAtom.patch _) (@δTerm.patch _).
  Definition patchIndices (δis : Δis) is :=
    δList.patch patchIndex δis is.

  Definition patchConstructor : Δc -> τc -> option τc :=
    δConstructor.patch.
  Definition patchConstructors (δcs : Δcs) cs :=
    δList.patch patchConstructor δcs cs.

  Definition patch (δ : Δ) : Inductive' Variable' -> option (Inductive' Variable') :=
    match δ with
    | Same => Some
    | Modify δn δps δis δcs =>
      fun '(Build_Inductive' n ps is cs) =>
        match
          (patchName δn n, patchParameters δps ps,
           patchIndices δis is, patchConstructors δcs cs)
        with
        | (Some n', Some ps', Some is', Some cs') =>
          Some (Build_Inductive' n' ps' is' cs')
        | _ => None
        end
    end.

End δInductive.

Axiom TODO : string -> δTerm.Δ Variable'.

Fixpoint nCpyApps ν n (b : δTerm.Δ ν) : δTerm.Δ ν :=
  match n with
  | 0 => b
  | S n' => δTerm.CpyApp (nCpyApps n' b) δTerm.Same
  end.

Fixpoint nCpyPis ν n (b : δTerm.Δ ν) : δTerm.Δ ν :=
  match n with
  | 0 => b
  | S n' => δTerm.CpyPi δTerm.Same δAtom.Same (nCpyPis n' b)
  end.

Fixpoint δquantifyParameters
         (δps : δInductive.Δps) (ps : VTerms Variable')
  : option (δTerm.Δ Variable' -> δTerm.Δ Variable')
  :=
  match δps with

  | δList.Insert (b, τ) δps =>
    (compose (δTerm.InsPi (δTerm.Replace τ) (Some b))) <$> δquantifyParameters δps ps

  | δList.Keep δps =>
    match ps with
    | [] => None
    | _ :: ps => (compose (δTerm.CpyPi δTerm.Same δAtom.Same)) <$> δquantifyParameters δps ps
    end

  | δList.Modify _ _ => Some (fun _ => TODO "δList.Modify")

  | δList.Same => Some (nCpyPis (List.length ps))

  end.

Definition option_apply A B (of : option (A -> B)) (x : A) : option B :=
  match of with
  | None => None
  | Some f => Some (f x)
  end.

Definition δeliminatorType
           (ind : Inductive' Variable') (δ : δInductive.Δ) : option (δTerm.Δ Variable') :=
  let '(Build_Inductive' n ps is cs) := ind in
  match δ with
  | δInductive.Same => Some δTerm.Same
  | δInductive.Modify δn δps δis δcs =>
    (* The outermost thing that happens is quantifying over the inductive parameters *)
    option_apply (δquantifyParameters δps ps)
                 $ δTerm.CpyPi δTerm.Same δAtom.Same
                 $ TODO "δInductive.Modify"
  end.

Theorem option_map_id : forall T (o : option T), id <$> o = o.
Proof.
  intros. destruct o; reflexivity.
Qed.

Theorem nCpyPis_quantifyVariables :
  ∀ l δrest rest,
    δTerm.patch (nCpyPis (List.length l) δrest) (quantifyVariables l rest)
    = option_map (quantifyVariables l) (δTerm.patch δrest rest).
Proof.
  induction l; intros.
  - simpl.
    unfold quantifyVariables.
    simpl.
    unfold quantifyBinders.
    unfold foldrWith.
    simpl.
    rewrite option_map_id.
    reflexivity.
  - destruct a as (v, t).
    simpl.
    unfold quantifyVariables in IHl.
    rewrite IHl.
    destruct (δTerm.patch δrest rest)eqn:PREST; simpl.
    + unfold quantifyVariables.
      simpl.
      reflexivity.
    + reflexivity.
Qed.

Theorem option_map_Some : forall A B (f : A -> B) x y,
  option_map f x = Some y ->
  exists inside, x = Some inside /\ f inside = y.
Proof.
  intros.
  destruct x as [some|].
  - exists some. simpl in *. split; congruence.
  - simpl in H. congruence.
Qed.

Theorem δquantifyParameters_correct :
  ∀ δps ps ps',
    δInductive.patchParameters δps ps = Some ps' ->
    ∀ δrest rest rest',
      δTerm.patch δrest rest = Some rest' ->
      δquantifyParameters δps ps δrest = Some δqps ->
      δTerm.patch δqps (quantifyVariables ps rest) = Some (quantifyVariables ps' rest').
Proof.
  unfold δInductive.patchParameters.
  intros δps.
  induction δps; intros ps ps' HI δrest rest rest' HT; simpl in *.

  - rewrite nCpyPis_quantifyVariables.
    rewrite HT.
    simpl.
    congruence.

  - destruct h as (b, τ).
    apply option_map_Some in HI.
    destruct HI as [ps'' [HI1 HI2]].
    subst ps'.
    simpl in *.
    erewrite IHδps; try eassumption.
    unfold quantifyVariables.
    simpl.
    reflexivity.

  - admit.

Admitted.

Definition δindIndexInside (δis : δInductive.Δis) : δTerm.Δ Variable' -> δTerm.Δ Variable' :=
  match δis with
  | δList.Same => nCpyApps (length is)
  end.

Definition δmkMotiveType (δn : δInductive.Δn) (δps : δInductive.Δps) (δis : δInductive.Δis)
  : δTerm.Δ Variable' :=
  δindIndexInside δi (TODO "δmkMotiveType").

Theorem δquantifyMotive_correct :
  ∀ δn δps δis δmotiveType,
    δmkMotiveType δn δps δis = δmotiveType ->
    ∀ δrest rest rest' motive motiveType motiveType',
      δTerm.patch δrest rest = Some rest' ->
      δTerm.patch δmotiveType motiveType = Some motiveType' ->
      δTerm.patch (δTerm.CpyPi δTerm.Same δAtom.Same δrest)
                  (quantifyVariables [(motive, motiveType)] rest)
      = Some (quantifyVariables [(motive, motiveType')] rest').
Proof.

Qed.

Theorem δeliminatorType_correct : ∀ i δi δt,
    δeliminatorType i δi = δt ->
    ∀ t i' t',
      eliminatorType i = t ->
      δInductive.patch δi i = Some i' ->
      δTerm.patch δt t = Some t' ->
      eliminatorType i' = t'.
Proof.
  intros i δi δt Hδe t i' t' He HδI HδT.
  destruct i as [n ps is cs].
  destruct i' as [n' ps' is' cs'].
  simpl in *.
  destruct δi; simpl in *.

  - inversion HδI; subst; clear HδI.
    simpl in HδT.
    inversion HδT; subst.
    reflexivity.

  - destruct (δInductive.patchName         δn   n)eqn:PN;  [ | congruence ].
    destruct (δInductive.patchParameters   δps ps)eqn:PPS; [ | congruence ].
    destruct (δInductive.patchIndices      δis is)eqn:PIS; [ | congruence ].
    destruct (δInductive.patchConstructors δcs cs)eqn:PCS; [ | congruence ].
    inversion HδI; subst; clear HδI.
    unfold eliminatorType' in *.

    erewrite δquantifyParameters_correct in HδT; try eassumption;
      [ inversion_clear HδT; reflexivity | ].
    clear HδT t'.




    simpl.

    *
      eapply H0. eassumption.
    * admit.

    * admit.

Admitted.
