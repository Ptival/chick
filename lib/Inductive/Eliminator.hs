module Inductive.Eliminator where

-- forall (A : Type) (P : forall n : nat, Vec A n -> Set),
--   P 0 (vnil A) ->
--   (forall (h : A) (n : nat) (t : t A n), P n t -> P (S n) (vcons A h n t)) ->
--   forall (n : nat) (t : Vec A n), P n t

-- The structure can be summarized as:
-- 1. quantify over the inductive parameters p1 p2
-- 2. quantify over the output property P
-- 3. for each constructor:
--   - quantify over all parameters cp1 cp2, but whenever the parameter is recursive,
--     add an appropriate P
--   - return (P ci1 ci2 (Constructor cp1 cp2))
-- 4. quantify over indices i1 i2
-- 5. quantify over one instance t of the input type (T ip1 ip2 i1 i2)
-- 6. return P i1 i2 t
eliminatorType' :: ∀ α.
  α ->
  Variable ->
  [(Variable, TermX α Variable)] ->
  [(Variable, TermX α Variable)] ->
  [(Variable, [(Variable, TermX α Variable)], [TermX α Variable])] ->
  TypeX α Variable
eliminatorType' α inductiveName inductiveParameters inductiveIndices constructors =

    quantifyVariables inductiveParameters
  $ quantifyVariables [(motive, motiveType)]
  $ quantifyCases
  $ quantifyVariables inductiveIndices
  $ quantifyVariables [(discriminee, discrimineeType)]
  $ outputType

  where

    -- apply :: [(Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
    applyTerm = foldlWith mkApp
    applyVar l = applyTerm (map (Var Nothing . fst) l)
    mkApp a t = App α a t

    -- quantifyVariables ::
    -- [(Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
    quantifyVariables l = quantifyBinders (map (\ (v, τ) -> (Binder (Just v), τ)) l)
    -- quantifyBinders ::
    -- [(Binder Variable, TermX α Variable)] -> TermX α Variable -> TermX α Variable
    quantifyBinders = foldrWith mkPi
    foldrWith f l a = foldr f a l
    mkPi (b, τ) a = Pi α τ (abstractBinder b a)

    discriminee :: IsString a => a
    discriminee = "instance"

    discrimineeType =
        applyVar inductiveIndices
      $ applyVar inductiveParameters
      $ Var Nothing inductiveName

    motive :: IsString a => a
    motive = "Motive"

    motiveType =
      mkMotiveType' α inductiveName inductiveParameters inductiveIndices Type

    outputType = App α (applyVar inductiveIndices motive) discriminee

    quantifyCases = foldrWith quantifyCase constructors

    quantifyCase (consName, consParameters, consIndices) acc =
      Pi α (mkCase consName consParameters consIndices) (abstractAnonymous acc)

    mkCase consName consParameters consIndices =
        quantifyBinders (concatMap addRecursiveMotive consParameters)
      $ applyTerm [applyVar consParameters (Var Nothing consName)]
      $ applyTerm consIndices
      $ motive

    -- if the term is `inductiveName` fully-applied, replace it with
    -- an instantiation of the motive
    addRecursiveMotive ::
      (Variable, TypeX α Variable) -> [(Binder Variable, TypeX α Variable)]
    addRecursiveMotive (v, τ) =
      case unpackFullyAppliedInductive τ of
        Just indices ->
          [ (Binder (Just v), τ)
          , (Binder Nothing, App α (applyTerm indices motive) (Var Nothing v))
          ]
        Nothing -> [(Binder (Just v), τ)]

    -- `acc` will contain the concrete indices, and will be well-sorted since
    -- we peel from the outermost application
    unpackFullyAppliedInductive' term nbParams nbIndices acc =
      case (term, nbParams, nbIndices) of
        (Var _ v, 0, 0) | v == inductiveName -> Just acc
                        | otherwise          -> Nothing
        (_, 0, 0) -> Nothing
        -- when ran out of indices, peel parameters
        (App _ l _, _, 0) ->
          unpackFullyAppliedInductive' l (nbParams - 1) nbIndices       acc
        (App _ l r, _, _) ->
          unpackFullyAppliedInductive' l nbParams       (nbIndices - 1) (r : acc)
        (_, _, _) -> Nothing

    unpackFullyAppliedInductive t =
      unpackFullyAppliedInductive' t
      (length inductiveParameters) (length inductiveIndices) []

eliminatorRawType :: Inductive Raw.Raw Variable -> Raw.Type Variable
eliminatorRawType (Inductive n ps is cs) =
  eliminatorType' () n ps (instantiateBinders "i" is) (instantiateConstructors cs)
  where
    instantiateConstructors = map instantiateConstructor
    instantiateConstructor (Constructor _ cn cps cis) =
      (cn, instantiateBinders "p" cps, cis)

eliminatorName :: Variable -> Variable
eliminatorName (Variable v) = Variable (v ++ "_rect")
