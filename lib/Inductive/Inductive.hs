{-# language FlexibleContexts #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module Inductive.Inductive where

import Bound.Name
import Control.Monad.Reader.Class
import Data.Default

import DictMetaOut
import Inductive.Constructor
import Precedence
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.PrettyPrintableUnannotated
import Term.Binder
import Term.Term
import Term.TypeChecked      as TypeChecked
import Term.Variable
import Text.PrettyPrint.Annotated.WL

data Inductive ξ ν =
  Inductive
  { name         :: ν
  , parameters   :: [(Binder ν, TypeX ξ ν)]
  , indices      :: [TypeX ξ ν]
  , constructors :: [Constructor ξ ν]
  }

deriving instance (ForallX Eq ξ, Eq ν) => Eq (Inductive ξ ν)
deriving instance (ForallX Show ξ, Show ν) => Show (Inductive ξ ν)

inductiveType ::
  [(Binder Variable, TypeChecked.Type Variable)] -> [TypeChecked.Type Variable] -> TypeChecked.Type Variable ->
  TypeChecked.Type Variable
inductiveType ps is o =
  foldr onParam (foldr onIndex o is) ps
  where
    onIndex :: TypeChecked.Type Variable -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onIndex i      t = Pi (Type ()) i (abstractAnonymous t)
    onParam :: (Binder Variable, TypeChecked.Type Variable) -> TypeChecked.Type Variable -> TypeChecked.Type Variable
    onParam (b, p) t = Pi (Type ()) p (abstractBinder b t)

arrows :: [Doc a] -> Doc a
arrows = encloseSep mempty mempty (text " →")

prettyBindingDocU ::
  MonadReader PrecedenceTable m =>
  (Binder Variable, TermX ξ Variable) -> m (Doc ())
prettyBindingDocU (Binder b, t) =
  case b of
    Nothing -> prettyDocU t
    Just v -> do
      tDoc <- prettyDocU t
      return $ parens . fillSep $
        [ prettyDoc v
        , text ":"
        , tDoc
        ]

instance
  (Default (X_App ξ), Default (X_Hole ξ), Default (X_Pi ξ)) =>
  PrettyPrintableUnannotated (Inductive ξ) where
  prettyDocU (Inductive n ps is cs) = do
    psDoc <- mapM prettyBindingDocU ps
    csDoc <- mapM (prettyConstructorDocU n ps) cs
    isDoc <- mapM prettyDocU is
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

prettyConstructorDocU ::
  (Default (X_App ξ), Default (X_Hole ξ), Default (X_Pi ξ)) =>
  MonadReader PrecedenceTable m =>
  Variable -> [(Binder Variable, TypeX ξ Variable)] ->
  Constructor ξ Variable -> m (Doc ())
prettyConstructorDocU ind indps (Constructor n ps is) = do
  -- it's annoying because to build the term we need annotations
  -- that we then discard when printing...
  cDoc <- prettyDocU (constructorTypeUnchecked ind indps ps is)
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
