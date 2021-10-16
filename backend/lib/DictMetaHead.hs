module DictMetaHead where

import Term.Term

data DictMetaHead a ξ = DictMetaHead
  { onAnnot :: X_Annot ξ -> a,
    onApp :: X_App ξ -> a,
    onHole :: X_Hole ξ -> a,
    onLam :: X_Lam ξ -> a,
    onLet :: X_Let ξ -> a,
    onPi :: X_Pi ξ -> a,
    onType :: X_Type ξ -> a,
    onVar :: a
  }

metaHead :: DictMetaHead a ξ -> TermX ξ ν -> a
metaHead d = \case
  Annot a _ _ -> onAnnot d a
  App a _ _ -> onApp d a
  Hole a -> onHole d a
  Lam a _ -> onLam d a
  Let a _ _ -> onLet d a
  Pi a _ _ -> onPi d a
  Type a -> onType d a
  Var _ -> onVar d
