module DictMetaLens where

-- import Control.Lens

-- import DictMeta
-- import DictMetaMap
-- import Term

-- newtype FlipGetter a b = FlipGetter { unFlipGetter :: Getter   b a }
-- newtype FlipSetter a b = FlipSetter { unFlipSetter :: ASetter' b a }

-- lensSetTerm ::
--   forall ξ v.
--   DictMeta (FlipSetter v) ξ ->
--   v -> TermX ξ -> TermX ξ
-- lensSetTerm ds v =
--   metaMap $ MkDictMetaMap
--   (modify (metaAnnot ds))
--   (modify (metaApp   ds))
--   (modify (metaHole  ds))
--   (modify (metaLam   ds))
--   (modify (metaLet   ds))
--   (modify (metaPi    ds))
--   (modify (metaType  ds))
--   (modify (metaVar   ds))
--   where
--     modify :: FlipSetter v t -> t -> t
--     modify s t = set (unFlipSetter s) v t

-- lensGetSetTerm ::
--   forall ξ i o.
--   DictMeta (FlipGetter i) ξ ->
--   DictMeta (FlipSetter o) ξ ->
--   (i -> o) -> TermX ξ -> TermX ξ
-- lensGetSetTerm dg ds f =
--   metaMap $ MkDictMetaMap
--   (modify (metaAnnot dg) (metaAnnot ds))
--   (modify (metaApp   dg) (metaApp   ds))
--   (modify (metaHole  dg) (metaHole  ds))
--   (modify (metaLam   dg) (metaLam   ds))
--   (modify (metaLet   dg) (metaLet   ds))
--   (modify (metaPi    dg) (metaPi    ds))
--   (modify (metaType  dg) (metaType  ds))
--   (modify (metaVar   dg) (metaVar   ds))
--   where
--     modify :: FlipGetter i t -> FlipSetter o t -> t -> t
--     modify g s t = set (unFlipSetter s) (f $ t ^. unFlipGetter g) t
