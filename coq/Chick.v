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

From Chick Require Import
     Diff.Atom
     Diff.List
     Notations
     Syntax.Term
.

Module δAtom := Chick.Diff.Atom.
Module δList := Chick.Diff.List.

Import ListNotations.
Open Scope string.

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

Definition mkApp l r : Term Variable' := App l r.

Definition mkPi '(b, l) r : Term Variable' := Pi l b r.

Definition foldlWith T A f (l : list T) (a : A) := List.fold_left f l a.

Definition foldrWith T A f (l : list T) (a : A) := List.fold_right f a l.

Definition applyTerm := foldlWith mkApp.

Definition applyVar (l : VTerms Variable') : Term Variable' -> Term Variable' :=
  applyTerm (List.map (@Var _ ∘ fst) l).

Definition quantifyBinders := foldrWith mkPi.

Definition quantifyVariables l : Term Variable' -> Term Variable' :=
  quantifyBinders (List.map (fun '(v, τ) => (Some v, τ)) l).

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
  foldrWith
    (quantifyCase inductiveName inductiveParameters inductiveIndices motive)
    constructors.

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

Definition onIndIndexInside t (i : VTerm Variable') := let '(v, _) := i in App t (Var v).

Eval compute in
    (
      List.fold_left onIndIndexInside [("a", Type'); ("b", Type')] Type'
    ).

Definition mkMotiveType'
           indName (indParams indIndices : VTerms Variable')
           universe :=

  let onIndIndexOutside '(v, p) t := Pi  p (Some v) t      in
  let onIndParam t '(b, _) := App t (Var b) in

  let inductive :=
    List.fold_left onIndIndexInside indIndices
                   $ List.fold_left onIndParam indParams
                   $ Var indName
  in

  List.fold_right
    onIndIndexOutside
    (Pi inductive None universe)
    indIndices

.

Definition
  eliminatorType'
  (inductiveName       : Variable')
  (inductiveParameters : VTerms Variable')
  (inductiveIndices    : VTerms Variable')
  (constructors        : list (Variable' * VTerms Variable' * list (Term Variable')))
  :=

    (*let inductiveIndices' := instantiateBinders "i" inductiveIndices in*)

    let discriminee := "instance" in
    let discrimineeTerm := Var discriminee in
    let discrimineeType :=
        applyVar inductiveIndices
                 $ applyVar inductiveParameters
                 $ Var inductiveName
    in

    let motive := "Motive" in
    let motiveTerm := Var motive in
    let motiveType :=
        mkMotiveType' inductiveName inductiveParameters inductiveIndices Type'
    in

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
  ; inductiveIndices      : VTerms ν (* should be BTerms but it's a PITA *)
  ; inductiveConstructors : list (Constructor ν)
  }.

Definition instantiateConstructor '(Build_Constructor cn cps cis) :=
  (cn, instantiateBinders "p" cps, cis).

Definition instantiateConstructors := List.map instantiateConstructor.

Definition eliminatorType (ind : Inductive' Variable') : Term Variable' :=
  let '(Build_Inductive' indName indParams indIndices indConstructors) := ind in

  eliminatorType' indName indParams indIndices
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
  | InsApp  : ∀ (δl : Δ ν) (δr : Δ ν), Δ ν
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

    | InsApp δl δr =>
      fun t =>
        match (patch δl t, patch δr t) with
        | (Some l', Some r') => Some (App l' r')
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

  (*
  Definition τi := BTerm Variable'.
  Definition Δi := δPair.Δ (δAtom.Δ (Binder Variable')) (δTerm.Δ Variable').
  Definition Δis := δList.Δ τi Δi.
   *)
  Definition τi := VTerm Variable'.
  Definition Δi := δPair.Δ (δAtom.Δ Variable') (δTerm.Δ Variable').
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
    ∀ δrest rest rest' δqps,
      δTerm.patch δrest rest = Some rest' ->
      δquantifyParameters δps ps = Some δqps ->
      δTerm.patch (δqps δrest) (quantifyVariables ps rest)
      = Some (quantifyVariables ps' rest').
Proof.
  unfold δInductive.patchParameters.
  intros δps.
  pose δps as δps_was.
  induction δps; intros ps ps' HI δrest rest rest' δqps HQ HT; simpl in *.

  - destruct h as (b, τ).
    apply option_map_Some in HI.
    apply option_map_Some in HT.
    destruct HI as [ps'' [HI1 HI2]].
    destruct HT as [δqps' [HT1 HT2]].
    subst ps' δqps.
    simpl in *.
    erewrite IHδps; try eassumption.
    unfold quantifyVariables.
    simpl.
    reflexivity.

  - destruct ps as [ | [v τ] ps ]; [ congruence | ].
    apply option_map_Some in HI.
    apply option_map_Some in HT.
    destruct HI as [ps'' [HI1 HI2]].
    destruct HT as [δqps' [HT1 HT2]].
    subst ps' δqps.
    simpl in *.
    unfold quantifyVariables in *.
    erewrite IHδps; try eassumption.
    reflexivity.

  - destruct ps as [ | [v τ] ps ]; [ congruence | ].
    admit.

  - inversion HT. subst δqps. clear HT.
    inversion HI. subst ps'. clear HI.
    rewrite nCpyPis_quantifyVariables.
    rewrite HQ.
    simpl.
    reflexivity.

Admitted.

Fixpoint δindIndexInside
         (is : VTerms Variable') (δis : δInductive.Δis) (base : δTerm.Δ Variable')
  : δTerm.Δ Variable' :=

  match δis with

  | δList.Insert (v, _) δis =>
    δindIndexInside is δis (δTerm.InsApp base (δTerm.Replace (Var v)))

  | δList.Keep δis =>
    δindIndexInside is δis (δTerm.CpyApp base δTerm.Same)

  | δList.Modify δpair δis =>
    match δpair with
    | δPair.Same => δindIndexInside is δis (δTerm.CpyApp base δTerm.Same)
    | δPair.Modify δl _ =>
      match δl with
      | δAtom.Same => δindIndexInside is δis (δTerm.CpyApp base δTerm.Same)
      | δAtom.Replace v =>
        δindIndexInside is δis (δTerm.CpyApp base (δTerm.Replace (Var v)))
      end
    end

  | δList.Same => nCpyApps (List.length is) base

  end.

Lemma δindIndexInside_correct :
  ∀ δis (is is' : VTerms Variable')
    δresult δbase,
    δindIndexInside is δis δbase = δresult ->
    δInductive.patchIndices δis is = Some is' ->
    ∀ base result base',
      List.fold_left onIndIndexInside is base = result ->
      δTerm.patch δbase base = Some base' ->
      ∀ result',
        List.fold_left onIndIndexInside is' base' = result' ->
        δTerm.patch δresult result = Some result'.
Proof.
  intros δis.
  pose δis as δis_was.
  induction δis; intros is is' δresult δbase DI PI base result base' FL PB result' FL'.

  - destruct h as [v t].
    subst δresult.
    simpl in *.
    unfold δInductive.patchIndices in PI.
    simpl in PI.
    apply option_map_Some in PI.
    destruct PI as [is'' [PI1 PI2]].
    subst is'.
    simpl in FL'.
    erewrite IHδis; eauto.
    simpl.
    rewrite PB.
    reflexivity.

  -



Qed.

Definition δmkMotiveType
           (δn : δInductive.Δn) (δps : δInductive.Δps) (δis : δInductive.Δis)
           (is : BTerms Variable')
  : δTerm.Δ Variable' :=
  δindIndexInside is δis (TODO "δmkMotiveType").

Theorem δmkMotiveType_correct :
  ∀ δn n n' δps ps ps' δis (is is' : BTerms Variable') (iis iis' : VTerms Variable')
    δcs cs cs' δmotiveType u,

    δmkMotiveType δn δps δis is = δmotiveType ->

    iis  = instantiateBinders "i" is ->
    iis' = instantiateBinders "i" is' ->

    δInductive.patch
      (δInductive.Modify δn δps δis δcs)
      (Build_Inductive' n ps is cs)
    = Some (Build_Inductive' n' ps' is' cs') ->

    δTerm.patch δmotiveType (mkMotiveType' n ps iis u)
    = Some (mkMotiveType' n ps' iis' u).

Proof.



Qed.

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
