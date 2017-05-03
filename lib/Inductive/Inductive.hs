{-# language FlexibleContexts #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Inductive.Inductive where

import Control.Monad.Reader.Class

import DictMetaOut
import Inductive.Constructor
import Precedence
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableAnnotated
import Term.Binder
import Term.Term
import Term.TypeChecked      as TypeChecked
import Term.Variable
import Text.PrettyPrint.Annotated.WL

data Inductive ξ =
  Inductive
  { name         :: Variable
  , parameters   :: [(Binder, TypeX ξ)]
  , indices      :: [TypeX ξ]
  , constructors :: [Constructor ξ]
  }

deriving instance (ForallX Eq   ξ) => Eq   (Inductive ξ)
deriving instance (ForallX Show ξ) => Show (Inductive ξ)

inductiveType ::
  [(Binder, TypeChecked.Type)] -> [TypeChecked.Type] -> TypeChecked.Type ->
  TypeChecked.Type
inductiveType ps is o =
  foldr onParam (foldr onIndex o is) ps
  where
    onIndex :: TypeChecked.Type -> TypeChecked.Type -> TypeChecked.Type
    onIndex i      t = Pi (Type ()) (Binder Nothing)  i t
    onParam :: (Binder, TypeChecked.Type) -> TypeChecked.Type -> TypeChecked.Type
    onParam (b, p) t = Pi (Type ()) b p t

arrows :: [Doc a] -> Doc a
arrows = encloseSep mempty mempty (text " →")

prettyBindingDoc ::
  MonadReader (DictMetaOut a TypeChecked, PrecedenceTable) m =>
  (Binder, TypeChecked.Term) -> m (Doc a)
prettyBindingDoc (Binder b, t) =
  case b of
    Nothing -> prettyDocA t
    Just v -> do
      tDoc <- prettyDocA t
      return $ parens . fillSep $
        [ prettyDoc v
        , text ":"
        , tDoc
        ]

instance PrettyPrintableAnnotated Inductive where
  prettyDocA (Inductive n ps is cs) = do
    psDoc <- mapM prettyBindingDoc ps
    csDoc <- mapM (prettyConstructorDoc n ps) cs
    isDoc <- mapM prettyDocA is
    return $ vsep $
      [ fillSep $
        [ text "inductive"
        , prettyDoc n
        ]
        ++
        (
          -- mempty creates an unwanted space, so have to use []
          if length ps == 0
          then []
          else [encloseSep mempty mempty mempty psDoc]
        )
        ++
        [ text ":"
        , arrows (isDoc ++ [text "Type"])
        , text "where"
        ]
      ] ++ (map (indent 2) csDoc)

prettyConstructorDoc ::
  MonadReader (DictMetaOut a TypeChecked, PrecedenceTable) m =>
  Variable -> [(Binder, TypeChecked.Type)] ->
  Constructor TypeChecked -> m (Doc a)
prettyConstructorDoc ind indps (Constructor n ps is) = do
  cDoc <- prettyDocA (constructorTypeChecked ind indps ps is)
  return $ fillSep
    [ prettyDoc n
    , text ":"
    , cDoc
      {-
, arrows (map (prettyBindingDoc dict precs) ps)
, text "→"
, prettyVariableDoc ind
, encloseSep mempty mempty mempty
(map (par precs (PrecApp, TolerateHigher) . prettyTermDocPrec dict precs) is)
-}
    ]
