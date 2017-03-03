{-# language LambdaCase #-}

module Term.Substitution where

import Term.Term

-- does NOT substitute in annotations!
subst :: Variable -> TermX ξ -> TermX ξ -> TermX ξ
subst v s = go
  where
    go = \case
      Annot a t τ -> Annot a (go t) (go τ)
      App a t1 t2 -> App   a (go t1) (go t2)
      Hole a -> Hole  a
      Lam a (Binder b) t
        | b == Just v -> Lam a (Binder b) t
        | otherwise  -> Lam a (Binder b) (go t)
      Let a (Binder b) t1 t2
        | b == Just v -> Let a (Binder b) (go t1) t2
        | otherwise  -> Let a (Binder b) (go t1) (go t2)
      Pi a (Binder b) τ1 τ2
        | b == Just v -> Pi a (Binder b) (go τ1) τ2
        | otherwise  -> Pi a (Binder b) (go τ1) (go τ2)
      Type a -> Type a
      Var a x
        | x == v     -> s
        | otherwise -> Var a v
