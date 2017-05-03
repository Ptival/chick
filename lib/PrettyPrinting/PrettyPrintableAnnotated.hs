{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PrettyPrinting.PrettyPrintableAnnotated where

import Control.Monad.Reader
import Data.Default
import Text.PrettyPrint.Annotated.WL

import DictMetaOut
import Precedence
import PrettyPrinting.PrettyPrintable
import PrettyPrinting.Utils
import Term.Binder
import Term.Term
import Term.Variable
import Term.TypeChecked

class PrettyPrintableAnnotated t where
  prettyDocA ::
    (MonadReader (DictMetaOut a TypeChecked, PrecedenceTable) m) =>
    t TypeChecked -> m (Doc a)
  prettyStrA :: t TypeChecked -> String
  prettyStrA t =
    display . renderPretty 1.0 80 . runReader (prettyDocA t) $
    (ignoreAnnotations, def)

instance PrettyPrintableAnnotated TermX where
  prettyDocA t = do
    (dict, precs) <- ask
    return $ par precs (PrecMin, TolerateEqual) . prettyTermDocPrec dict precs $ t

prettyTermDocPrec ::
  forall a ξ. DictMetaOut a ξ -> PrecedenceTable -> TermX ξ -> (Doc a, Precedence)
prettyTermDocPrec dict precs = goTerm

  where

    go :: (Precedence, Tolerance) -> TermX ξ -> Doc a
    go pt = par precs pt . prettyTermDocPrec dict precs

    goTerm :: TermX ξ -> (Doc a, Precedence)
    goTerm term =
      (\ (doc, p) -> (annotate (metaOut dict term) doc, p))
      $ case term of

      Annot _ t τ ->
        (fillSep
         [ go (PrecAnnot, TolerateHigher) t
         , text annotSymbol
         , go (PrecAnnot, TolerateHigher) τ
         ]
        , PrecAnnot)

      App _ t1 t2 ->
        (fillSep
         [ go (PrecApp, TolerateEqual) t1
         , go (PrecApp, TolerateHigher) t2
         ]
        , PrecApp)

      Hole _ -> (text holeSymbol, PrecAtom)

      l@(Lam _ _ _) -> (goLams [] l, PrecLam)

      Let _ n t1 t2 ->
        (fillSep
         [ text "let"
         , prettyDoc n
         , char '='
         , go (PrecMin, TolerateEqual) t1
         , text "in"
         , go (PrecLet, TolerateEqual) t2
         ]
        , PrecLet)

      Pi _ (Binder Nothing) τ1 τ2 ->
        (fillSep
         [ go (PrecArrow, TolerateHigher) τ1
         , char '→'
         , go (PrecArrow, TolerateEqual) τ2
         ]
        , PrecArrow)

      Pi _ (Binder (Just (Variable n))) τ1 τ2 ->
        (fillSep
         [ parens $ fillSep
           [ text n
           , char ':'
           , go (PrecMin, TolerateEqual) τ1
           ]
         , char '→'
         , go (PrecArrow, TolerateEqual) τ2
         ]
        , PrecArrow)

      Type _ -> (text "Type", PrecAtom)

      Var _ v -> (prettyDoc v, PrecAtom)

    goLams :: [Doc a] -> TermX ξ -> Doc a
    goLams l = \case
      Lam _ n t -> goLams (prettyDoc n : l) t
      t -> fillSep
          [ char 'λ'
          , fillSep . reverse $ l
          , char '.'
          , go (PrecMin, TolerateEqual) t
          ]
