{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module WellFormed where

import Parsing (reservedWords)
import Term.Binder
import Term.Term
import Term.Raw as Raw
import Term.Variable

{-
foldVars :: forall a ξ. (Variable -> a -> a) -> TermX ξ -> a -> a
foldVars g = go
  where
    go = \case
      Annot _ t τ     -> go τ  . go t
      App   _ t1 t2   -> go t2 . go t1
      Hole  _         -> id
      Lam   _ b t     -> go t . g' b
      Let   _ b t1 t2 -> go t2 . go t1 . g' b
      Pi    _ b τ1 τ2 -> go τ2 . go τ1 . g' b
      Type  _         -> id
      Var   _ x       -> g x
    g' :: Binder -> a -> a
    g' (Binder (Just v)) = g v
    g' _                 = id

wellFormed :: Raw.Term -> Bool
wellFormed t =
  let condition (Variable v) =
        -- Variable name should not be empty
        v /= ""
        -- Variable name should not be reserved
        && notElem v reservedWords
  in
  foldVars (\ v acc -> acc && condition v) t True
-}
