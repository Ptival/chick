-- module Term.Fold where

-- import Control.Arrow
-- import Term.Term

-- data DictTermFold a ξ = DictTermFold
--   { doAnnot :: X_Annot ξ -> TermX ξ -> TypeX ξ -> a -> a,
--     doApp :: X_App ξ -> TermX ξ -> TypeX ξ -> a -> a,
--     doHole :: X_Hole ξ -> a -> a,
--     doLam :: X_Lam ξ -> Binder -> TermX ξ -> a -> a,
--     doLet :: X_Let ξ -> Binder -> TermX ξ -> TermX ξ -> a -> a,
--     doPi :: X_Pi ξ -> Binder -> TypeX ξ -> TypeX ξ -> a -> a,
--     doType :: X_Type ξ -> a -> a,
--     doVar :: X_Var ξ -> Variable -> a -> a
--   }

-- -- prefix fold
-- termFold :: forall a ξ. DictTermFold a ξ -> TermX ξ -> a -> a
-- termFold d = go
--   where
--     go :: TermX ξ -> a -> a
--     go = \case
--       Annot a t τ -> doAnnot d a t τ >>> go t >>> go τ
--       App a t1 t2 -> doApp d a t1 t2 >>> go t1 >>> go t2
--       Hole a -> doHole d a
--       Lam a b t -> doLam d a b t >>> go t
--       Let a b t1 t2 -> doLet d a b t1 t2 >>> go t1 >>> go t2
--       Pi a b τ1 τ2 -> doPi d a b τ1 τ2 >>> go τ1 >>> go τ2
--       Type a -> doType d a
--       Var a v -> doVar d a v
